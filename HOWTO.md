## Continuous integration 

1- document and commit change with GitKraken

2- execute use_version() - this will commit a new change

3- execute clean and install

4- build source package (recommandation)

5- go to Tuser and renv::install(build_location) - this will be under C:\Users\aldoh\Documents\RApplication


## Intégration dans le package Tdata

_Placer les fichiers_

* fin_logger.py dans inst/python/
* logger.R dans R/

_Dans le fichier .onload_

* Assurez-vous qu'il appelle tdata_setup_logger() en premier
* Pour réduire les logs d'ib_insync, utilisez tdata_setup_logger("INFO", control_ibinsync = TRUE, ibinsync_level = "ERROR")


_Dans vos modules Python_

* Importez fin_logger au début: import fin_logger
* Créez un logger spécifique: logger = fin_logger.get_logger("tdata_py.votre_module")
* Utilisez le logger: logger.info("Message", {"contexte": valeur})
* Mesurez le temps d'exécution avec le décorateur: @fin_logger.log_execution_time


_Dans vos fonctions R_

* Utilisez les fonctions t_log: t_log_info("Message")
* Capturez les exceptions: tryCatch({ ... }, error = function(e) { tdata_log_exception(e, "Message d'erreur") })
* Mesurez le temps d'exécution: fonction_temporisée <- tdata_log_execution_time(fonction_originale)
