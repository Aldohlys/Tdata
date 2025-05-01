
import socket
import random
from ib_insync import *

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

def getPort(silent=True):
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
            if (not silent): print("Port id:", port_id)
            return port_id
        print(f"Port {port_id} is in use, trying another port...")
    
    # If we've tried max_attempts times and found no open port
    raise RuntimeError(f"Could not find an available port after {max_attempts} attempts")

def safe_ib_connect(host='127.0.0.1', port=7496, client_id=None, readonly=False, silent=True):
    """
    Safely connect to Interactive Brokers with proper error handling.
    
    Args:
        host (str): IB host address
        port (int): IB port number
        client_id (int): Client ID for the connection, if None a random port is used
        readonly (bool): Whether to use readonly mode
        silent (bool): Whether to suppress connection error messages
        
    Returns:
        IB instance - The IB instance
    """
    if client_id is None:
        client_id = getPort(silent)
        
    ib = IB()
    try:
        ib.connect(host, port, clientId=client_id, readonly=readonly)
    except Exception as e:
        if not silent:
            print(f"IB connection error: {str(e)}")

    return ib

def isIBAvailable(silent=True):
    """
    Check if Interactive Brokers TWS/Gateway is available by attempting a connection.
    
    Args:
        silent (bool): Whether to suppress connection error messages
    
    Returns:
        bool: True if connection successful, False otherwise
    """
    # Use safe_ib_connect instead of direct connection
    ib= safe_ib_connect(silent=silent)

    # Check if connection was successful before proceeding

    # If connection not available return None
    if not ib.isConnected(): 
        if not silent:
            print("Interactive Brokers is not available")
        return False
    
    ib.disconnect()
    ib.sleep(0)
    return True

