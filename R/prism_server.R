## v0.3.0 2019-05-27

get_my_name<-function()
{
  x<-getPackageName()
  return(x)
}


thisSession<-new.env()

thisSession$redis_connection_status<-0  #0:not connected; 1:connected
thisSession$REDIS_ADDRESS="prism.resp.core.ubc.ca"
thisSession$REDIS_PORT <- 3001

thisSession$MODE_REQUIRE_SESSION=TRUE;  #If TRUE, connect_to_model will return a sessionId (necessary for async calls!)
thisSession$MODE_REQUIRE_SESSION_DATA=FALSE;

thisSession$MODE_ASYNC <- 2; #0: Only runs sync, 1: only runs async. 2:Runs both sync and async

thisSession$MODEL_DESCRIPTION<-paste0("This is ",get_my_name()," - PRISM enabled!")
thisSession$MODEL_VERSION<-paste(packageVersion(get_my_name()))

thisSession$message=""; #For passing messages to client when needed (functions can return status code, but fill this variable for more information)

thisSession$session_id <- ""


prism_redis_connect <- function (){
  if(thisSession$redis_connection_status==0)
  {
    rredis::redisConnect(host = thisSession$REDIS_ADDRESS, port = thisSession$REDIS_PORT, password = "H1Uf5o6326n6C2276m727cU82O")
    thisSession$redis_connection_status<-1
  }
}









#' @export
gateway<-function(...)
{
  arguments=list(...)
  func<-arguments$func

  session_id<-arguments$session_id
  if(is.null(session_id)) session_id=""
  thisSession$session_id<-session_id

  arguments$func<-NULL
  arguments$session_id<-NULL

  if(length(arguments)==0) {
    out<-eval(parse(text=paste(func,"()")))
  }
  else {
    out<-do.call(func, args = arguments)
  }


  return(jsonlite::toJSON(out))
}






#' @export
prism_model_run<-function(model_input=NULL)
{
  if(thisSession$MODE_ASYNC==1)
  {
    #Model can only run in asyn mode!
    thisSession$message <- "Synchronous model run is not enabled for this model. Try model_run.async instead"
    return(list(error_code=1, error_message=thisSession$message))
  }

  return(model_run(model_input))
}











#In API-based use without session ids this might seem a bit reduntant (it will not be required). But still good to check model availability
connect_to_model<-function()
{
  model_name<-environmentName(environment(connect_to_model))
  out<-list(error_code=0,session_id="",version="",description="")

  if(thisSession$MODE_REQUIRE_SESSION)
  {
    session_id<-generate_id()
    out$session_id<-session_id
  }

  out$version<-thisSession$MODEL_VERSION
  out$description<-thisSession$MODEL_DESCRIPTION
  return(out)
}




#Creates generic unique strings for multiple purposes
generate_id<-function()
{
  id<-paste(c(sample(letters,1) , sample(c(letters,0:9),9,TRUE)),collapse="")
  return(id)
}



#' #' @export
#' prism_get_output_structure<-function()
#' {
#'   out<-list(
#'     n_agents=prism_output(source="$n_agents", type = "numeric/scalar", group = "", title = "Number of simulated individuals", description = ""),
#'   )
#'   return(out)
#' }












set_redis_var<-function(variable,value)
{
  #TODO: connect should not be in these functions as it will cause multiple connection attempts!
  prism_redis_connect()
  rredis::redisSet(variable,value)
  return(TRUE)
}


get_redis_var<-function(variable)
{
  prism_redis_connect()
  x<-rredis::redisGet(variable)
  return(x)
}



delete_redis_var<-function(variable)
{
  prism_redis_connect()
  rredis::redisDelete(variable)
}



set_var<-function(variable,value)
{
  .GlobalEnv[[variable]]<-value
}


get_var<-function(variable)
{
  return(.GlobalEnv[[variable]])
}

