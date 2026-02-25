
import socket
import random
from ib_async import *
from fin_logger import get_logger, DEBUG, INFO
from tdata_py._core import CONFIG

# Get logger for this module
logger = get_logger()

# Read IBKR API port from config (default: 7496)
_ibkr_config = CONFIG.get("ibkr", {})
DEFAULT_IB_PORT = int(_ibkr_config.get("api_port", 7496)) if isinstance(_ibkr_config, dict) else 7496

def is_port_in_use(port):
    """
    Check if a port is already in use on localhost.
    
    Args:
        port (int): The port number to check
        
    Returns:
        bool: True if port is in use, False otherwise
    """
    import socket
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        return s.connect_ex(('localhost', port)) == 0

def getPort():
    """
    Generate a random available port for IB connection.
    Keeps trying until it finds an available port.
    
    Returns:
        int: An available port number
    """
    import random
    max_attempts = 100  # Prevent infinite loops
    
    for _ in range(max_attempts):
        port_id = random.randint(1, 9990)
        if not is_port_in_use(port_id):
            logger.debug("Port selected successfully", context = {"Port id": port_id})
            return port_id
        logger.debug(f"Port {port_id} is in use, trying another port...")
    
    # If we've tried max_attempts times and found no open port
    raise RuntimeError(f"Could not find an available port after {max_attempts} attempts")

def safe_ib_connect(host='127.0.0.1', port=None, client_id=None, readonly=False):
    """
    Safely connect to Interactive Brokers with proper error handling.
    
    Args:
        host (str): IB host address
        port (int): IB port number
        client_id (int): Client ID for the connection, if None a random port is used
        readonly (bool): Whether to use readonly mode

    Returns:
        IB instance - The IB instance
    """
    if port is None:
        port = DEFAULT_IB_PORT
    if client_id is None:
        client_id = getPort()

    ib = IB()
    
    try:
        ib.connect(host, port, clientId=client_id, readonly=readonly)
        if logger.logger.level <= DEBUG:
            logger.debug(f"Successfully connected to IB at {host}:{port}", context={
                "client_id": client_id,
                "readonly": readonly })
        else:
            logger.info("Successfully connected to IB")
        
    except Exception:  # Don't need 'as e' since exception captures it
        logger.exception("IB connection failed", context={
            "host": host,
            "port": port,
            "client_id": client_id
        })
    finally:
        return ib

def isIBAvailable():
    """
    Check if Interactive Brokers TWS/Gateway is available by attempting a connection.
    
    Returns:
        bool: True if connection successful, False otherwise
    """
    # Use safe_ib_connect instead of direct connection
    ib= safe_ib_connect()

    # Check if connection was successful before proceeding
    if not ib.isConnected():
        logger.info("IB connection not available", context={
            "function": "isIBAvailable",
            "status": "disconnected"
        })
        return False
    
    # Connection successful
    logger.info("IB connection available", context={
        "function": "isIBAvailable", 
        "status": "connected"
    })
    ib.disconnect()
    ib.sleep(0)
    return True

