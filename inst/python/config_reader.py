# config_reader.py
import yaml
import os

class RConfigLoader(yaml.SafeLoader):
    """Custom YAML loader that handles R-specific tags"""
    pass

def expr_constructor(loader, node):
    """Handle !expr tags by returning the string value"""
    return loader.construct_scalar(node)

# Register the !expr tag
RConfigLoader.add_constructor('!expr', expr_constructor)

def read_config(config_path=None):
    """
    Read configuration from config.yml file
    
    Args:
        config_path: Path to config file. If None, uses R environment variable
    
    Returns:
        dict: Configuration dictionary
    """
    # Get config environment from R_CONFIG_ACTIVE or default to 'default'
    ## config_env = os.environ.get('R_CONFIG_ACTIVE', 'default')
    config_env = "default"
    
    # If config_path is None, search in common locations
    if config_path is None:
        # Common locations to check, matching R implementation
        possible_paths = [
            os.path.join(os.environ.get("USERPROFILE", ""), "config.yml"),  # Windows user directory
            os.path.join(os.path.expanduser("~"), "config.yml"),           # Cross-platform home directory
            os.path.join(os.getcwd(), "config.yml"),                       # Current working directory
            os.path.join(os.environ.get("R_CONFIG_DIR", ""), "config.yml"), # R_CONFIG_DIR env variable
            os.path.join(os.getcwd(), "conf", "config.yml"),               # conf subdirectory
            os.path.join(os.getcwd(), "inst", "config.yml"),               # inst directory for packages
            # Additional Python-specific paths
            os.path.join(os.path.dirname(__file__), "config.yml"),         # Same directory as this file
            os.path.join(os.path.dirname(__file__), "..", "config.yml"),   # Parent directory
            os.path.join(os.path.dirname(__file__), "..", "..", "config.yml")  # Two levels up
        ]
        
        # Find first existing file
        for path in possible_paths:
            if os.path.exists(path):
                config_path = path
                print(f"[PY] Found config file at: {config_path}")
                break
    
    if config_path is None or not os.path.exists(config_path):
        print("[PY] No config file found")
        return None
    
    # Read YAML file
    try:
        with open(config_path, 'r') as f:
            all_config = yaml.load(f, Loader=RConfigLoader)  # Use custom loader
    except Exception as e:
        print(f"[PY] Error reading config file: {e}")
        return None
    
    # Get ONLY the specific environment section - no merging
    config = all_config.get(config_env, {})
    
    return config

def merge_dicts(base, override):
    """Recursively merge two dictionaries"""
    result = base.copy()
    for key, value in override.items():
        if key in result and isinstance(result[key], dict) and isinstance(value, dict):
            result[key] = merge_dicts(result[key], value)
        else:
            result[key] = value
    return result

def get_logging_config(config=None):
    """Extract logging configuration from config dict"""
    
    # If no config provided, try to read from file
    if config is None:
        config = read_config()
        # If reading failed, return None
        if config is None:
            return None
    
    # Get Python logging configuration
    logging_config = config.get('logging', {})
    python_config = logging_config.get('python', {})
    
    return {
        'level': python_config.get('level', 'INFO'),
        'control_ibasync': python_config.get('control_ibasync', False),
        'ibasync_level': python_config.get('ibasync_level', 'ERROR'),
        'log_dir': logging_config.get('log_dir'),
    }
