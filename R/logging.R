#' #' @title Generic Package Logging Functions
#' #' @description Generic logging functionality that can be used in any T* package
#' #' with automatic package name detection
#'
#' #' @title Obtenir le nom du package actuel
#' #' @keywords internal
#' .get_current_package_name <- function() {
#'   # Méthode 1: Utiliser packageName() si on est dans un namespace de package
#'   pkg_name <- tryCatch({
#'     packageName()
#'   }, error = function(e) {
#'     NULL
#'   })
#'
#'   if (!is.null(pkg_name) && pkg_name != ".GlobalEnv") {
#'     return(pkg_name)
#'   }
#'
#'   # Méthode 2: Examiner la pile d'appels pour trouver le namespace
#'   calls <- sys.calls()
#'   for (i in rev(seq_along(calls))) {
#'     call_env <- tryCatch({
#'       environment(sys.function(i))
#'     }, error = function(e) {
#'       NULL
#'     })
#'
#'     if (!is.null(call_env)) {
#'       env_name <- environmentName(call_env)
#'       if (startsWith(env_name, "namespace:")) {
#'         return(sub("^namespace:", "", env_name))
#'       }
#'     }
#'   }
#'
#'   # Méthode 3: Examiner les frames pour trouver un namespace de package
#'   frames <- sys.frames()
#'   for (i in rev(seq_along(frames))) {
#'     frame <- frames[[i]]
#'     if (exists(".__NAMESPACE__.", frame, inherits = FALSE)) {
#'       ns_env <- frame
#'       pkg_name <- environmentName(ns_env)
#'       if (startsWith(pkg_name, "namespace:")) {
#'         return(sub("^namespace:", "", pkg_name))
#'       }
#'     }
#'   }
#'
#'   # Méthode 4: Si on est dans un environnement de package chargé avec devtools
#'   if (exists(".packageName", parent.frame(), inherits = TRUE)) {
#'     pkg_name <- get(".packageName", parent.frame(), inherits = TRUE)
#'     if (!is.null(pkg_name)) {
#'       return(pkg_name)
#'     }
#'   }
#'
#'   # Méthode 5: Chercher dans les parents de l'environnement actuel
#'   env <- parent.frame(2)
#'   while (!is.null(env) && !identical(env, emptyenv())) {
#'     env_name <- environmentName(env)
#'     if (startsWith(env_name, "namespace:") || startsWith(env_name, "package:")) {
#'       return(sub("^(namespace:|package:)", "", env_name))
#'     }
#'     env <- parent.env(env)
#'   }
#'
#'   # Si aucune méthode ne fonctionne, retourner "unknown"
#'   return("unknown")
#' }
#'
#' #' @title Log debug message in package context
#' #' @param message Message to log
#' #' @param context Named list of contextual information
#' #' @export
#' t_log_debug <- function(message, context = NULL) {
#'   package_name <- .get_current_package_name()
#'   tryCatch({
#'     Tbasics::t_log_debug(message, context, module = package_name)
#'   }, error = function(e) {
#'     return(invisible(FALSE))
#'   })
#' }
#'
#' #' @title Log info message in package context
#' #' @param message Message to log
#' #' @param context Named list of contextual information
#' #' @export
#' t_log_info <- function(message, context = NULL) {
#'   package_name <- .get_current_package_name()
#'   tryCatch({
#'     Tbasics::t_log_info(message, context, module = package_name)
#'   }, error = function(e) {
#'     return(invisible(FALSE))
#'   })
#' }
#'
#' #' @title Log warning message in package context
#' #' @param message Message to log
#' #' @param context Named list of contextual information
#' #' @export
#' t_log_warn <- function(message, context = NULL) {
#'   package_name <- .get_current_package_name()
#'   tryCatch({
#'     Tbasics::t_log_warn(message, context, module = package_name)
#'   }, error = function(e) {
#'     return(invisible(FALSE))
#'   })
#' }
#'
#' #' @title Log error message in package context
#' #' @param message Message to log
#' #' @param context Named list of contextual information
#' #' @export
#' t_log_error <- function(message, context = NULL) {
#'   package_name <- .get_current_package_name()
#'   tryCatch({
#'     Tbasics::t_log_error(message, context, module = package_name)
#'   }, error = function(e) {
#'     return(invisible(FALSE))
#'   })
#' }
#'
#' #' @title Log exception in package context
#' #' @param e exception object to be passed by calling tryCatch block
#' #' @param message Message to log
#' #' @param context Named list of contextual information
#' #' @export
#' t_log_exception <- function(e, message = "Exception occurred", context = NULL) {
#'   package_name <- .get_current_package_name()
#'   tryCatch({
#'     t_log_exception(e, message, context, module = package_name)
#'   }, error = function(err) {
#'     return(invisible(FALSE))
#'   })
#' }
#'
#' #' @title Wrap a function to log its execution time
#' #'
#' #' @description Create a wrapper that logs the start and completion of a function
#' #' with the execution time in minutes and seconds.
#' #'
#' #' @param func The function to wrap
#' #' @param func_name Optional name for the function (defaults to function name)
#' #' @return Wrapped function that logs execution time
#' #' @export
#' t_log_execution_time <- function(func, func_name = NULL) {
#'   package_name <- .get_current_package_name()
#'   tryCatch({
#'     # Pass to Tbasics with current package module
#'     Tbasics::t_log_execution_time(func, func_name, module = package_name)
#'   }, error = function(e) {
#'     # Si erreur, retourner la fonction originale
#'     return(func)
#'   })
#' }
