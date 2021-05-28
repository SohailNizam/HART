#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
A script that takes in csv files prepared either by HAL.R or l1_experiments.R, 
creates an anytree object that represents a HART regression tree, and writes
a new csv file containing information about the trees.
"""

#import necessary libraries
from anytree import  Node
#from anytree import AnyNode, RenderTree
#from anytree.exporter import DotExporter
import random
#import copy
import pandas as pd
import sys




################## Creating the fit objects #########################

def create_dict(df):
    
    '''
    Create the hal_dict object from the R hal object
    Input: A pandas data frame containing  3 columns 
          (col numbers, split values, beta coefficients)
          
    Output: A dictionary(keys = strings like "x11", values = list(label, beta))
    Example: {"x11" : [x1>5, 1.5]}
    '''
    
    #initialize an empty dict
    hal_dict = {}
    
    #iterate over all rows of the input df
    #there will be one dict entry per row
    for index, row in df.iterrows():
        
        #First deal with interaction terms.
        #all int terms will have a "c" in them
        #a hold over from them being stored in R vectors
        
        if "(" in row["var_name"]:
            
            
            
            #int term "var_name" strings look like "c("var1", "var2")"
            #remove the leading "c(" and the trailing ")" and extra "s
            #leaves us with just "var1, var2"
            var = (row["var_name"].replace("c(", "")).replace(")", "").replace('"', "")
            
            #split on "," to get list ["var1", "var2"]
            var = var.split(",")
            
            
            #The "cutoff" values look the same. Long decimals instead of ints
            #again remove "c(" and ")"
            val = (row["cutoffs"].replace("c(", "")).replace(")", "")
            #split into list
            val = val.split(",")
            
            #initialize label and key strings
            #label will be displayed in the tree node (part of the dict val)
            label = ""
            #key will hold the corresponding value from hal_fit
            #this will link the two objects
            #this is the dict key
            key = ""
            
            
            #iterate over both lists
            #construct the label and key strings
            for name, cut in zip(var, val):
                label += name  + ">" + cut + ","

                key += name + "_" + cut + ","

            
            #remove extra trailing comma from each
            label = label[:-1]
            key = key[:-1]


        #if not an interaction term 
        else:
            
            #same deal but only one value, so no iteration
            #want to turn 67.0 into 67
            #but keep 67.5 as 67.5
            if str(int(float(row["cutoffs"]))) == str(row["cutoffs"]):
                string = str(int(row["cutoffs"]))
            else:
                string = str(row["cutoffs"])
            
            
            label = row["var_name"] + ">" + string
            key = row["var_name"] + "_" + string
        
        
        #remove all spaces in strings for easier parsing in grow_tree()
        label = label.replace(" ", "")
        key = key.replace(" ", "")
            
        
        #add the label and beta value to the dict in a list
        #with key as the dict key
        hal_dict[key] = [label, row["coeffs"]]
    
    return(hal_dict)
    


def create_fit(features):
    
    '''
    Create the hal_fit object (list of lists of strings).
    This object will be iterated over with each item acting as
    the key in hal_dict. 
    
    Input: Features from original dataset
    Output: List of lists of strings    
    '''
    
    #initialize the empty hal_fit object
    hal_fit = []
    
    #remove the index column from features
    features = features.drop(features.columns[0], axis = 1)
    
    #for each feature in the original dataset
    for name, var in features.iteritems():
        
        #remove duplicates and sort
        #turn into list
        vals = list((var.drop_duplicates()).sort_values())
    
    
        #now make it a list of strings that includes
        #the variable name
        val_strings = []
        for val  in vals:
            
            
            #want to turn 67.0 into 67
            #but keep 67.5 as 67.5
            if int(val) == val:
                string = str(int(val))
            else:
                string = str(val)
                
                
            val_strings.append(name + '_' +  string)
            
            
        
        #add this variable to hal_fit
        hal_fit.append(val_strings)
        
    
    return(hal_fit)

    
    
def create_value_dict(hal_fit, hal_dict):
    '''
    Create a dict of all of the main effects values
    that need to be involved in the tree.
    Keys are the strings in hal_fit like "x1_1".
    Values are lists of all the interaction terms that cause this term
    to be in the dictionary. So the list may be empty
    if the term is only there because of its main effect beta.
    
    Items appear in this dict either because their 
    main effect coefficient is nonzero
    or because they appear in an interaction term whose
    coefficient is nonzero.
    
    Input: hal_fit object (list of lists of strings)
           hal_dict object
    Output: Dictionary
    '''
    
    
    #initialize the list to hold the important vals
    value_list = []
    
    #go through all of the hal_dict keys
    for val, attr in hal_dict.items():
        if "," in val and attr[1] != 0:
            value_list += val.split(",")
    
        elif "," not in val and attr[1] != 0:
            value_list.append(val)
    
    #initialize the final dictionary to be returned
    #with value_list as the keys
    value_dict = dict.fromkeys(value_list, 0)
    
    
    #iterate over all of the hal_dict keys
    for key in hal_dict.keys():
        
        #if the key is an interaction term
        if "," in key and hal_dict[key][1] != 0:
            #split the term into a list
            key_split = key.split(",")

            #append the original interaction term
            #to the list corresponding to each main effect
            #involved in it
            for item in key_split:
                if item in value_dict.keys():
                    if value_dict[item] == 0:
                        value_dict[item] = [key]
                    else:
                        value_dict[item].append(key)

            
            #so "x1_1,x2_1" will be added to the list
            #corresponding to both "x1_1" and "x2_1"
            #Now we know why each term is actually in
            #value_dict
    
    #also go through and add each of the values
    #themselves to the list of "reasons for being present"
    #if necessary
    for val in value_dict.keys():
 
            
        if hal_dict[val][1] != 0 and value_dict[val] == 0:
            value_dict[val] = [val]
            
        elif hal_dict[val][1] != 0 and value_dict[val] != 0:
            value_dict[val].append(val)
    
    return(value_dict)


def update_val_dict(hal_fit, value_dict):
    
    
    #update value_list by removing all terms that are only in there
    #because of an interaction that relies on this value
    #or a value greater than it within the same var
    
    
    #first copy value_dict so original is unaltered
    new_value_dict = value_dict.copy()
    #initialize a list of values to remove
    vals_to_rmv = []
        
        
    #iterate over all of the values within the current variable
    #so if at x2_3, go over all values in x2 (will start at x2_3)
    for i in range(len(hal_fit[0])):
            
        #iterate over new_value_dict keys
        for val in new_value_dict.keys():
            #set a boolean = True
            remove_val = True
            
            #don't remove the term itself
            if val == hal_fit[0][i]:
                remove_val = False
            
            #iterate over the list associated with that value
            for term in new_value_dict[val]:
                
                #if any of the terms in the list
                #do not contain the current tree value
                #change our boolean to false
                if hal_fit[0][i] not in term:
                    remove_val = False
                    
                    #we only need one term to not contain our value
                    break
            
            #if remove_val has been changed to false,
            #it means there's a reason to keep it
            if remove_val:
                vals_to_rmv.append(val)
        
        
    for val in list(set(vals_to_rmv)):
        del new_value_dict[val]
    
    return(new_value_dict)


def clean_hal(hal_fit, value_dict):
    
    #takes hal_fit object
    #and removes values not needed in tree
    #ie values not in value_dict.keys()
    
    nearly_clean_fit = []
    for var in hal_fit:
        var_list = []
        for val in var:
            if val in value_dict.keys():
                var_list.append(val)
        nearly_clean_fit.append(var_list)
    
    #remove any empty lists
    clean_fit = []
    for var in nearly_clean_fit:
        if len(var) != 0:
            clean_fit.append(var)
            
    return(clean_fit)


def sort_hal(hal_fit, value_dict):
    
    #takes hal_fit and orders the variables
    #by the number of values that would be removed if
    #the first value of that variable were selected
    
    num_vals = len(value_dict.keys())
    num_removed_list = []


    for ind, var in enumerate(hal_fit):
        
        num_removed = 0
        
        #move that var to first position in hal_fit
        new_hal_fit = hal_fit.copy()
        new_hal_fit = [new_hal_fit.pop(ind)] + new_hal_fit
        

        val_dict_left = update_val_dict(new_hal_fit, value_dict)
        num_vals_left = len(val_dict_left.keys())
        
        num_removed += num_vals - num_vals_left
        num_removed += len(var) + 1

        
        num_removed_list.append(num_removed)
    
    
    if len(num_removed_list) > 0:
        best_index = num_removed_list.index(max(num_removed_list))
        sorted_fit = [hal_fit.pop(best_index)] + hal_fit
    
    else:
        sorted_fit = hal_fit
    
    

    return(sorted_fit)



################## Grow the regression tree #########################
    
def shrink_hal(hal_fit, direction):
    '''
    Shrinks a hal fit object (list of lists)
    appropriately according to which direction we're moving.
    
    Input: hal fit object (list of lists of strings), direction (string)
    Output: shrunken hal_fit (list of lists of strings, possibly empty)
    '''
    
    #if moving left, remove current variable entirely
    #ie remove first list in hal object
    if direction == "left":
        
        shrunk_hal = hal_fit[1:]
    
    #if moving right, only remove top element of first list
    elif direction == "right":
        
        if len(hal_fit[0][1:]) > 0:
            shrunk_hal = [hal_fit[0][1:]] + hal_fit[1:]
        #if there's only one element left in the first list
        #this is equivalent to entirely removing the current variable
        else:
            shrunk_hal = hal_fit[1:]
    
    return(shrunk_hal)




def build_gt_list(hal_fit, end_val):
    
    '''
    Get a list of values that a certain value is "greater than"
    in a hal_fit.
    
    Input: hal_fit (list of lists of strings), end_val (string)
    Output: List of strings
    '''
    
    #return list of elements "up to and including"
    #specified value in hal_fit. Only include non int terms
    #except for the end_val which can be an int term and will be added
    
    gt_list = []
    for var in hal_fit:
        for val in var:
            #only add non interaction terms to gt_list
            #except for the end_val which will be added either way
            #but its individual components will be added
            if val == end_val:
                #iterate over  from start of that variable list
                #to end_val, not inclusive
                gt_list += var[:var.index(end_val) + 1]
                
                break
            
            #TODO
            #break out of both loops
                
    return(gt_list)
    


def grow_tree(hal_fit, hal_dict, hal_fit2, value_dict, total_beta = 0, gt = [], k = 2):
    
    '''
    The main function in this process.
    Recursively grow a hal tree.
    
    Input: hal_fit object (list of lists to be shrunk with each recursive call)
           hal_fit2 (exact copy of hal fit kept the same in each iter)
           hal_dict (dictionary linking split names to beta values)
           value_dict (dictionary linking data values to the terms in the basis
                       expansion causing them to be relevant in the given 
                       tree region)
           total_beta (running prediction total)
           gt (list containing labels of all values we're currently 
               "greater than")
           k (int, the degree of interaction term used to fit hal)
           
    Output: an anytree node object with children
    '''
    

    #base case
    #if the fit is empty, return a node object
    if len(hal_fit) == 0:
        
        
        #every node needs a unique id
        #anytree merges nodes with "name" attributes
        idnum = str(random.uniform(0,1))
        
        #we've reached a terminal node
        #add the appropriate interaction term betas
        for term, coeff in hal_dict.items():
            if "," in term and coeff[1] != 0:
                if all(item in gt for item in term.split(",")):
                    total_beta += coeff[1]

        
        #return the node
        return(Node(name = idnum, 
                    display_name = str(total_beta),
                    pred = total_beta,
                    gt = gt))
    
    
    #recursive case 1: beta = 0 and term not in any signif int terms
    #(ie not in value_list)
    #don't create a node out of the top value
    #shrink hal_fit and recusrively call the function
    elif hal_fit[0][0] not in value_dict.keys():
        
        hal_fit_shrunk = shrink_hal(hal_fit, "right")
        node = grow_tree(hal_fit_shrunk, 
                         hal_dict,
                         hal_fit2,
                         value_dict,
                         total_beta = total_beta,
                         gt = gt,
                         k = k)
        return(node)
    
    
    
    #recursive case 2: beta != 0 or term in signif int term
    #(ie is in value list)
    #create node out of top value and give it 2 children
    elif hal_fit[0][0] in value_dict.keys(): 
        
        #create a parent node out of the first value in the hal object
        #the random number is a unique identifier 
        #needed to avoid the graphviz merged nodes problem
        node = Node(name = str(random.uniform(0,1)), 
                    #display_name = hal_dict[hal_fit[0][0]][0],
                    display_name = hal_fit[0][0],
                    beta = hal_dict[hal_fit[0][0]][1],
                    total_beta = total_beta,
                    gt = gt)
        
        #prune from value_dict for left-hand sub-region after node creation
        new_value_dict = update_val_dict(hal_fit, value_dict)
        
        #create a left branch out of the parent node
        #remove the whole first variable
        hal_fit_left_shrunk = shrink_hal(hal_fit, "left")
        

        #now update hal_fit based on new_value_dict
        hal_fit_left_shrunk = clean_hal(hal_fit_left_shrunk, new_value_dict)
        
        #sort hal_fit_left_shrunk
        hal_fit_left_shrunk = sort_hal(hal_fit_left_shrunk, new_value_dict)
        
       
        
        #nothing added to running beta total for left moves
        left_child = grow_tree(hal_fit_left_shrunk, 
                                   hal_dict,
                                   hal_fit2,
                                   new_value_dict,
                                   total_beta = total_beta,
                                   gt = gt,
                                   k = k)
        
        #assign new child/subtree to its parent node
        left_child.parent = node
        
        
        #create a right branch out of the parent node
        #remove just the first value of the hal object
        hal_fit_right_shrunk = shrink_hal(hal_fit, "right")
        

        #sort hal_fit_right_shrunk
        hal_fit_right_shrunk = sort_hal(hal_fit_right_shrunk, value_dict)
        
        #add the parent node beta to the running total
        new_total = total_beta + node.beta
        
        #add the parent node value to the gt list 
        new_gt =  node.gt + build_gt_list(hal_fit2, hal_fit[0][0])
        
        #grow the subtree with new attributes
        right_child = grow_tree(hal_fit_right_shrunk, 
                                hal_dict,
                                hal_fit2,
                                value_dict,
                                total_beta = new_total,
                                gt = new_gt,
                                k = k)
        
        #assign this child/subtree to its parent node                       
        right_child.parent = node

        return(node)


##################### Evaluate Trees ##################### 



def build_n_count(data_name, l1, num_fits):
    
    
    '''
    This function builds trees from hal fit information generated in R
    and counts their terminal nodes and unique terminal nodes.
    
    Input: data_name (str, the name of the dataset used to fit hal)
           l1 (boolean, True if building trees for l1 penalty term experiments)
           num_fits (int, the number of hal fit files to build trees for)
           
    Output: node_count_df (pandas df, contains columsn with terminal node
                           counts and unique terminal node counts)
    
    '''
    
    tn_counts = []
    utn_counts = []
    features = pd.read_csv('./data/' + data_name + "_features.csv")
    
    if l1:
        file_type = '_l1_'
    else:
        file_type = '_hal_'
    
    for i in range(1, num_fits + 1):
                
        #read in data
        data = pd.read_csv('./data/' + data_name + file_type + str(i) + '.csv')
        
        #create all objects needed for grow_tree
        hal_dict = create_dict(data)
        hal_fit2 = create_fit(features)
        value_dict = create_value_dict(hal_fit2, hal_dict)
        hal_fit = clean_hal(hal_fit2, value_dict)
        hal_fit = sort_hal(hal_fit, value_dict)
        
        
        #grow the tree
        #num cols - 1 to ignore the index column
        tree = grow_tree(hal_fit, hal_dict, hal_fit2, 
                         value_dict, k = features.shape[1] - 1)
        
       # print("tree", i, "built in", time.time() - t0, "seconds")
        
        #append terminal node counts
        tn_counts.append(len(tree.leaves))
        
        #count unique terminal nodes
        utn_count = 0
        utn_list = []
        for leaf in tree.leaves:
            if leaf.pred not in utn_list:
                utn_count += 1
                utn_list.append(leaf.pred)
                
        #append unique terminal node counts
        utn_counts.append(utn_count)
        
    
    #save all counts as a pandas df
    node_count_df = pd.DataFrame({"tn_counts" : tn_counts,
                     "utn_counts" : utn_counts})
    
    
    return(node_count_df)



#get command line arguments: l1, data_name, and num_fits
l1 = int(sys.argv[1])
data_name = sys.argv[2]
num_fits = int(sys.argv[3])

if l1:
    file_type = '_l1_'
else:
    file_type= '_hart_'

#call the build_n_count function
node_counts = build_n_count(data_name = data_name, 
                            l1 = l1, num_fits = num_fits)

#read in the corresponding r2 data
r2_data = pd.read_csv('./data/' + data_name + file_type + 'r2s.csv')

#combine r2 data with node count data into one df
node_counts['r2'] = r2_data['r2']

#write the new complete df to csv (to the current working dir)
node_counts.to_csv('./results/' + data_name + file_type + 'eval.csv',  index = False)



