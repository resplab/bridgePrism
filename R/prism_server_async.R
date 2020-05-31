#Contains async functionality for PRISM server
#By Mohsen Sadatsafavi
#2020.05.30






#' @export
prism_model_run_async<-function(model_input=NULL)
{
  x<-check_async_privilege()
  if(isFALSE(x))
  {
    thisSession$message <- paste("Error: Asynchornous call was not successful.",thisSession$message)
  }

  async_token<-generate_id()

  #Set the status to 0, and put the model input in redis and wait for the workhorse to pick it up
  set_redis_var(paste0("AS:status:",async_token),"[READY_TO_START]")
  set_redis_var(paste0("AS:status_time:",async_token),as.numeric(Sys.time()))
  set_redis_var(paste0("AS:status_data:",async_token),list(model_name=getPackageName(),model_input=model_input))
  return(list(error_code=0, async_token=async_token))
}







#This function will only be called from within the model (not from the client)
#' @export
prism_set_progress<-function(value)
{
  set_redis_var(paste0("AS:status:",thisSession$async_token),"[IN_PROGRESS]")
  set_redis_var(paste0("AS:status_time:",thisSession$async_token),as.numeric(Sys.time()))
  set_redis_var(paste0("AS:status_data:",thisSession$async_token),value)
}







#' @export
prism_check_async_status<-function(async_token)
{
  status<-get_redis_var(paste0("AS:status:",async_token))

  if(is.null(async_token))
  {
    return(list(error_code=-1, error_message=paste("There is no asynchronous model running for token",async_token)))
  }

  status_time<-get_redis_var(paste0("AS:status_time:",async_token))
  status_data<-get_redis_var(paste0("AS:status_data:",async_token))

  return(list(status=status,status_time=status_time,status_data=status_data))
}








#Returns FALSE if there is a reason not to grant async privilege to this lad
check_async_privilege<-function()
{
  if(thisSession$MODE_ASYNC==0)
  {
    thisSession$message<-"This model does not allow asynchronous communication"
    return(FALSE)
  }

  x <- get_redis_var(paste0("AS:", thisSession$sessionId))
  if(!is.null(x))
  {
    thisSession$message<-"Concurrent async call within one session are not allowed"
    return(FALSE)
  }

  return(TRUE)
}
