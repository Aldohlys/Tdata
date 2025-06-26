# Step 1: Add the correct path to Python
cat("=== ADDING TDATA PYTHON PATH ===\n")

# The path should be the inst/python directory
tdata_python_path <- file.path(getwd(), "inst", "python")
cat("Adding path:", tdata_python_path, "\n")

# Normalize the path for Windows
tdata_python_path_normalized <- normalizePath(tdata_python_path, winslash = "/")
cat("Normalized path:", tdata_python_path_normalized, "\n")

# Add to Python sys.path
py_run_string(sprintf("
import sys
tdata_path = r'%s'
if tdata_path not in sys.path:
    sys.path.insert(0, tdata_path)
    print(f'✓ Added {tdata_path} to Python path')
else:
    print(f'ℹ {tdata_path} already in Python path')
print(f'Python path now has {len(sys.path)} entries')
", tdata_python_path_normalized))

