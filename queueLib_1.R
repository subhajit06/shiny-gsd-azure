### test reading/writing queue with JSON object
### OCtober 23, 2020
### @ Subhajit Sengupta
library(jsonlite)
library(AzureQstor)

## https://select-statistics.co.uk/calculators/sample-size-calculator-two-proportions/

### credential from url and key
return_url_key <- function(){
  
  url = "YYYY"
  key = "YYYYYYY"
  return(list(url,key))
}

### queue creation
create_queue_by_name <- function(qname){
  
  info = return_url_key()
  qendp  = queue_endpoint(info[[1]], info[[2]])  
  
  q = create_storage_queue(qendp, qname) ### what if another queue with same name exists ?
  
  return(q)                                              ### should we check with list_storage_queue ??
}

### do we need function to delete queue ?


### make json object from parameter and send to a queue
put_params_in_queue <- function(q, paramList){
  
  in_msg = paramList
  in_json_obj = toJSON(in_msg)
  q$put_message(in_json_obj)
}

### retrieve json object from a queue and pass param list
### by default detele that msg but keep an option with flag
### azure automatically +1 to Dequeue count after each get 
get_params_from_queue <- function(q, delMsg=1){
  
  out_json_obj = q$get_message() 
  out_json_obj_text = out_json_obj$text
  
  if (length(out_json_obj_text)==0){
    stop("error in msg retrieving!!")
  }
  
  if(delMsg == 1){
    q$delete_message(out_json_obj)
  }
  
  paramList = fromJSON(out_json_obj_text)  
  return (paramList)
}

###################################################
###################################################


