# diagnostic.py
# Save this to /inst/python/diagnostic.py in your R package
# Then run with: reticulate::py_run_file(system.file("python/diagnostic.py", package="Tdata"))

import sys
import os

def check_module_structure(module_dir):
    print(f"Checking directory: {module_dir}")
    
    # Check if directory exists
    if not os.path.exists(module_dir):
        print(f"ERROR: Directory {module_dir} does not exist")
        return
    
    # List all files in the directory
    files = os.listdir(module_dir)
    print(f"Files in directory: {files}")
    
    # Check for __init__.py
    if "__init__.py" not in files:
        print("WARNING: No __init__.py found in directory")
    else:
        init_path = os.path.join(module_dir, "__init__.py")
        print(f"Found __init__.py at {init_path}")
        
        # Read the first few lines to check imports
        try:
            with open(init_path, 'r') as f:
                lines = f.readlines()
                print("First 20 lines of __init__.py:")
                for i, line in enumerate(lines[:20]):
                    print(f"{i+1}: {line.rstrip()}")
        except Exception as e:
            print(f"ERROR reading __init__.py: {str(e)}")
    
    # Check for expected module files
    expected_files = ["IB_connection.py", "contract.py", "account.py", 
                     "interest_rate_utils.py", "dividend_utils.py"]
    
    for file in expected_files:
        if file in files:
            print(f"✓ Found {file}")
        else:
            print(f"✗ Missing {file}")
    
    # If there's a tdata_py subdirectory, check that too
    tdata_subdir = os.path.join(module_dir, "tdata_py")
    if os.path.exists(tdata_subdir) and os.path.isdir(tdata_subdir):
        print("\nFound tdata_py subdirectory, checking it:")
        check_module_structure(tdata_subdir)

# Print Python path
print("Python sys.path:")
for p in sys.path:
    print(f"  {p}")

# Print current directory
print(f"Current directory: {os.getcwd()}")

# Try to import the module
print("\nAttempting to import tdata_py...")
try:
    import tdata_py
    print(f"Success! Module located at: {tdata_py.__file__}")
    print(f"Module attributes: {dir(tdata_py)}")
except ImportError as e:
    print(f"Import failed: {str(e)}")
    
    # Try to find the module in possible locations
    possible_locations = [
        os.path.join(p, "tdata_py") for p in sys.path if os.path.exists(os.path.join(p, "tdata_py"))
    ]
    
    print(f"Possible tdata_py locations found: {len(possible_locations)}")
    for loc in possible_locations:
        check_module_structure(loc)

# Check the actual import path for key files
print("\nChecking if IB_connection.py can be imported directly...")
try:
    import IB_connection
    print(f"IB_connection.py found at: {IB_connection.__file__}")
except ImportError as e:
    print(f"IB_connection.py import failed: {str(e)}")
