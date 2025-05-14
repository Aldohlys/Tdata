"""
fin_logger.py - Lightweight logging for financial applications

Ce module fournit des fonctionnalités de logging cohérentes et légères
pour les applications financières utilisant Python et R.
"""

import logging
import sys
import os
import time
import inspect
import json
from datetime import datetime
from config_reader import get_logging_config


# Implémentation personnalisée de wraps
def wraps(wrapped):
    """
    Décorateur simplifié qui imite functools.wraps
    
    Args:
        wrapped: La fonction à décorer
        
    Returns:
        Décorateur qui copie les métadonnées de la fonction wrapped
    """
    def decorator(wrapper):
        # Copie des attributs importants
        for attr in ['__module__', '__name__', '__qualname__', '__doc__', '__annotations__']:
            if hasattr(wrapped, attr):
                setattr(wrapper, attr, getattr(wrapped, attr))
        
        # Marquer comme étant un wrapper
        wrapper.__wrapped__ = wrapped
        return wrapper
    return decorator

# Configuration globale
_GLOBAL_CONFIG = {
    "initialized": False,
    "default_level": "ERROR",
    "loggers": {},
}

def setup_logging_from_config(config_path=None, **kwargs):
    """
    Setup logging using configuration from config.yml
    
    Args:
        config_path: Path to config.yml file
        **kwargs: Additional arguments to override config values
    
    Returns:
        logging.Logger: Configured logger
    """
    # Get configuration
    config = get_logging_config()
    
    if config is None:
        # Fallback to defaults if config not found
        config = {
            'level': 'INFO',
            'control_ibinsync': False,
            'ibinsync_level': 'ERROR'
        }
    
    # Override with any provided kwargs
    config.update(kwargs)

    # Debug: Print what we got from config
    print(f"[PY] Logging config loaded: {config}")

    # Determine log file path
    log_file = None
    if config.get('log_dir') and config.get('daily_log'):
        # Create daily log file
        log_dir = config['log_dir']
        if not os.path.exists(log_dir):
            os.makedirs(log_dir, exist_ok=True)
        
        # Create filename with date
        today = datetime.now().strftime('%Y%m%d')
        log_file = os.path.join(log_dir, f't_system_{today}.log')
        
        if not os.path.exists(log_dir):
            os.makedirs(log_dir, exist_ok=True)
    
    # Setup logging
    logger = setup_logging(
        level=config.get('level', 'INFO'),
        log_file=log_file,
        add_timestamp=True,
        reset_handlers=True
    )
    
    # Configure ib_insync logging if requested
    if config.get('control_ibinsync', False):
        print(f"[PY] Configuring ib_insync logging to level: {config.get('ibinsync_level', 'ERROR')}")
        configure_ibinsync_logging(config.get('ibinsync_level', 'ERROR'))
    else:
        print("[PY] ib_insync control not enabled in config")
    
    return logger

def setup_logging(level="ERROR", log_file=None, add_timestamp=True, 
                  reset_handlers=True, propagate=False, fmt=None):
    """
    Configure le système de logging Python
    
    Args:
        level: Niveau de log (TRACE, DEBUG, INFO, WARN, ERROR, FATAL)
        log_file: Chemin optionnel vers un fichier de log
        add_timestamp: Ajouter un timestamp aux messages
        reset_handlers: Supprimer les handlers existants
        propagate: Propager les logs aux loggers parents
        fmt: Format personnalisé pour les logs
    
    Returns:
        logging.Logger: Logger racine configuré
    """
    # Convertir niveau string en niveau Python
    level_map = {
        "TRACE": logging.DEBUG,  # Python n'a pas de niveau TRACE
        "DEBUG": logging.DEBUG,
        "INFO": logging.INFO,
        "WARN": logging.WARNING,
        "WARNING": logging.WARNING,
        "ERROR": logging.ERROR,
        "FATAL": logging.CRITICAL,
        "CRITICAL": logging.CRITICAL
    }
    
    py_level = level_map.get(level.upper(), logging.INFO)
    
    # Stocker le niveau par défaut
    _GLOBAL_CONFIG["default_level"] = level.upper()
    
    # Configurer le logger racine
    logger = logging.getLogger()
    logger.setLevel(py_level)
    logger.propagate = propagate
    
    # Supprimer les handlers existants si demandé
    if reset_handlers:
        for handler in logger.handlers[:]:
            logger.removeHandler(handler)
    
    # Créer un formateur
    if fmt is None:
        if add_timestamp:
            formatter = logging.Formatter(
                '[%(asctime)s] [PY] [%(levelname)s] (%(name)s) %(message)s',
                '%Y-%m-%d %H:%M:%S'
            )
        else:
            formatter = logging.Formatter(
                '[PY] [%(levelname)s] (%(name)s) %(message)s'
            )
    
    # Ajouter le handler console si aucun handler n'existe
    if len(logger.handlers) == 0:
        console = logging.StreamHandler(sys.stdout)
        console.setFormatter(formatter)
        logger.addHandler(console)
    
    # Ajouter un handler fichier si spécifié
    if log_file:
        # Créer le répertoire si nécessaire
        log_dir = os.path.dirname(log_file)
        if log_dir and not os.path.exists(log_dir):
            os.makedirs(log_dir, exist_ok=True)
            
        file_handler = logging.FileHandler(log_file)
        file_handler.setFormatter(formatter)
        logger.addHandler(file_handler)
    
    _GLOBAL_CONFIG["initialized"] = True
    return logger

def set_all_loggers_level(level):
    """
    Configure le niveau de tous les loggers Python existants
    
    Args:
        level: Niveau de log (TRACE, DEBUG, INFO, WARN, ERROR, FATAL)
    """
    # Convertir niveau string en niveau Python
    level_map = {
        "TRACE": logging.DEBUG,  # Python n'a pas de niveau TRACE
        "DEBUG": logging.DEBUG,
        "INFO": logging.INFO,
        "WARN": logging.WARNING,
        "WARNING": logging.WARNING,
        "ERROR": logging.ERROR,
        "FATAL": logging.CRITICAL,
        "CRITICAL": logging.CRITICAL
    }
    
    py_level = level_map.get(level.upper(), logging.INFO)
    
    # Configurer le logger racine
    root_logger = logging.getLogger()
    root_logger.setLevel(py_level)
    
    # Configurer tous les loggers existants
    for logger_name in logging.root.manager.loggerDict:
        logging.getLogger(logger_name).setLevel(py_level)
    
    return True

def configure_ibinsync_logging(level="ERROR"):
    """
    Configure spécifiquement le niveau de log des modules ib_insync
    
    Args:
        level: Niveau de log (TRACE, DEBUG, INFO, WARN, ERROR, FATAL)
    """
    # Convert level string to Python level
    level_map = {
        "TRACE": logging.DEBUG,
        "DEBUG": logging.DEBUG,
        "INFO": logging.INFO,
        "WARN": logging.WARNING,
        "WARNING": logging.WARNING,
        "ERROR": logging.ERROR,
        "FATAL": logging.CRITICAL,
        "CRITICAL": logging.CRITICAL
    }
    
    py_level = level_map.get(level.upper(), logging.ERROR)
    
    # Configure all ib_insync loggers comprehensively
    ib_modules = [
        'ib_insync',
        'ib_insync.wrapper',
        'ib_insync.client',  
        'ib_insync.ib',
        'ib_insync.contract',
        'ib_insync.order',
        'ib_insync.util'
    ]
    
    for module in ib_modules:
        logging.getLogger(module).setLevel(py_level)
    
    # Configurer tous les loggers ib_insync
    for logger_name in logging.root.manager.loggerDict:
        if logger_name.startswith('ib_insync'):
            logging.getLogger(logger_name).setLevel(py_level)
    
    return True

def get_logger(module_name=None):
    """
    Obtient ou crée un logger pour un module spécifique
    
    Args:
        module_name: Nom du module (utilise le module appelant par défaut)
        
    Returns:
        FinLogger: Logger pour le module
    """
    # Initialiser le logging si pas encore fait
    if not _GLOBAL_CONFIG["initialized"]:
        setup_logging(_GLOBAL_CONFIG["default_level"])
    
    # Obtenir le nom du module depuis l'appelant si non fourni
    if module_name is None:
        # Obtenir la frame appelante
        frame = inspect.currentframe().f_back
        module = inspect.getmodule(frame)
        
        if module:
            module_name = module.__name__
        else:
            # Utiliser un nom générique si module non trouvé
            module_name = "fin_app"
    
    # Réutiliser le logger existant si disponible
    if module_name in _GLOBAL_CONFIG["loggers"]:
        return _GLOBAL_CONFIG["loggers"][module_name]
    
    # Créer un nouveau logger et le mettre en cache
    logger = FinLogger(module_name)
    _GLOBAL_CONFIG["loggers"][module_name] = logger
    
    return logger

def log_with_context(level, message, context=None, module=None):
    """
    Log un message avec contexte additionnel
    
    Args:
        level: Niveau de log (TRACE, DEBUG, INFO, WARN, ERROR, FATAL)
        message: Message à logger
        context: Dictionnaire de contexte additionnel
        module: Nom du module pour catégoriser les logs
    """
    # Obtenir le nom du module depuis l'appelant si non fourni
    if module is None:
        # Obtenir la frame appelante
        frame = inspect.currentframe().f_back
        module_obj = inspect.getmodule(frame)
        
        if module_obj:
            module = module_obj.__name__
        else:
            # Utiliser un nom générique si module non trouvé
            module = "fin_app"
    
    # Obtenir le logger pour le module
    logger = logging.getLogger(module)
    
    # Formater le contexte si fourni
    if context:
        # Convertir les objets en strings si nécessaire
        safe_context = {}
        for k, v in context.items():
            if isinstance(v, (str, int, float, bool, type(None))):
                safe_context[k] = v
            else:
                try:
                    safe_context[k] = str(v)
                except:
                    safe_context[k] = repr(v)
        
        context_str = ", ".join([f"{k}={v}" for k, v in safe_context.items()])
        full_message = f"{message} {{{context_str}}}"
    else:
        full_message = message
    
    # Mapper le niveau au method
    log_fn = {
        "TRACE": logger.debug,  # Python n'a pas de niveau TRACE
        "DEBUG": logger.debug,
        "INFO": logger.info,
        "WARN": logger.warning,
        "WARNING": logger.warning,
        "ERROR": logger.error,
        "FATAL": logger.critical,
        "CRITICAL": logger.critical
    }.get(level.upper(), logger.info)
    
    # Logger le message
    log_fn(full_message)

class FinLogger:
    """Classe de logger pour applications financières"""
    
    def __init__(self, module_name):
        """
        Initialise un logger pour un module spécifique
        
        Args:
            module_name: Nom du module
        """
        # Obtenir logger
        self.logger = logging.getLogger(module_name)
        self.module_name = module_name
    
    def log(self, level, message, context=None):
        """
        Log un message avec contexte au niveau spécifié
        
        Args:
            level: Niveau de log (TRACE, DEBUG, INFO, WARN, ERROR, FATAL)
            message: Message à logger
            context: Dictionnaire d'informations contextuelles
        """
        log_with_context(level, message, context, self.module_name)
    
    # Méthodes de convenance pour chaque niveau
    def trace(self, message, context=None):
        """Log au niveau TRACE (correspont à DEBUG en Python)"""
        self.log("TRACE", message, context)
    
    def debug(self, message, context=None):
        """Log au niveau DEBUG"""
        self.log("DEBUG", message, context)
    
    def info(self, message, context=None):
        """Log au niveau INFO"""
        self.log("INFO", message, context)
    
    def warn(self, message, context=None):
        """Log au niveau WARN"""
        self.log("WARN", message, context)
    
    def warning(self, message, context=None):
        """Log au niveau WARNING (alias pour warn)"""
        self.log("WARNING", message, context)
    
    def error(self, message, context=None):
        """Log au niveau ERROR"""
        self.log("ERROR", message, context)
    
    def fatal(self, message, context=None):
        """Log au niveau FATAL"""
        self.log("FATAL", message, context)
    
    def critical(self, message, context=None):
        """Log au niveau CRITICAL (alias pour fatal)"""
        self.log("CRITICAL", message, context)
    
    def exception(self, message="Exception occurred", context=None, 
                  exc_info=True, stack_limit=None):
        """
        Log l'exception courante avec stack trace
        
        Args:
            message: Message à logger
            context: Dictionnaire de contexte additionnel
            exc_info: Inclure les informations d'exception
            stack_limit: Limiter la profondeur de la stack trace
        """
        # Obtenir les infos d'exception
        exc_type, exc_value, exc_tb = sys.exc_info()
        
        if exc_type:
            # Obtenir les détails de l'exception
            exc_msg = str(exc_value)
            
            # Ajouter au contexte
            ctx = context or {}
            ctx.update({
                "exception_type": exc_type.__name__,
                "exception_message": exc_msg
            })
            
            # Formater la stack trace si demandé
            if stack_limit is not None and exc_tb:
                try:
                    import traceback
                    stack = traceback.format_tb(exc_tb, limit=stack_limit)
                    ctx["stack_trace"] = "\n".join(stack)
                except:
                    # Gérer les erreurs de traceback
                    pass
            
            # Logger avec traceback
            if exc_info:
                self.logger.exception(
                    f"{message} {{{', '.join([f'{k}={v}' for k, v in ctx.items() if k != 'stack_trace'])}}}"
                )
            else:
                self.error(message, ctx)
        else:
            # Pas d'exception active
            self.error(f"{message} (no active exception)", context)

def log_execution_time(function=None, logger=None, function_name=None):
    """
    Décorateur pour logger le temps d'exécution d'une fonction
    
    Peut être utilisé comme @log_execution_time ou avec paramètres
    
    Args:
        function: Fonction à décorer
        logger: Instance de FinLogger (optionnel)
        function_name: Nom optionnel à utiliser pour la fonction dans les logs
    """
    # Cas où le décorateur est utilisé sans arguments
    if function is not None:
        # Créer logger par défaut si non fourni
        if logger is None:
            module_name = function.__module__ if hasattr(function, '__module__') else None
            logger = get_logger(module_name)
        
        # Utiliser le nom réel de la fonction si non spécifié
        if function_name is None:
            function_name = function.__name__
            
        @wraps(function)
        def wrapper(*args, **kwargs):
            start_time = time.time()
            
            try:
                # Log début
                logger.debug(f"Starting {function_name}")
                
                # Exécuter la fonction
                result = function(*args, **kwargs)
                
                # Calculer durée
                duration_ms = round((time.time() - start_time) * 1000, 2)
                
                # Log complétion
                logger.debug(
                    f"Function {function_name} completed", 
                    {"duration_ms": duration_ms}
                )
                
                return result
            except Exception as e:
                # Calculer durée même en cas d'erreur
                duration_ms = round((time.time() - start_time) * 1000, 2)
                
                # Log erreur avec durée
                logger.error(
                    f"Error in {function_name}", 
                    {
                        "duration_ms": duration_ms,
                        "error": str(e)
                    }
                )
                
                # Relancer l'exception
                raise
                
        return wrapper
    
    # Cas où le décorateur est utilisé avec des arguments
    else:
        def decorator(function):
            # Utiliser le nom réel de la fonction si non spécifié
            nonlocal function_name
            if function_name is None:
                function_name = function.__name__
                
            # Créer logger par défaut si non fourni
            nonlocal logger
            if logger is None:
                module_name = function.__module__ if hasattr(function, '__module__') else None
                logger = get_logger(module_name)
            
            @wraps(function)
            def wrapper(*args, **kwargs):
                start_time = time.time()
                
                try:
                    # Log début
                    logger.debug(f"Starting {function_name}")
                    
                    # Exécuter la fonction
                    result = function(*args, **kwargs)
                    
                    # Calculer durée
                    duration_ms = round((time.time() - start_time) * 1000, 2)
                    
                    # Log complétion
                    logger.debug(
                        f"Function {function_name} completed", 
                        {"duration_ms": duration_ms}
                    )
                    
                    return result
                except Exception as e:
                    # Calculer durée même en cas d'erreur
                    duration_ms = round((time.time() - start_time) * 1000, 2)
                    
                    # Log erreur avec durée
                    logger.error(
                        f"Error in {function_name}", 
                        {
                            "duration_ms": duration_ms,
                            "error": str(e)
                        }
                    )
                    
                    # Relancer l'exception
                    raise
                    
            return wrapper
        
        return decorator

# Initialiser le logging par défaut lors de l'import
if not _GLOBAL_CONFIG["initialized"]:
    setup_logging()
