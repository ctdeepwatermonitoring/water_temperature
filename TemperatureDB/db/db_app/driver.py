import subprocess
import sys
from pathlib import Path

# Use Path objects for cross-platform compatibility
BASE_DIR = Path.home() / "deepuser"
CONT_DIR = BASE_DIR / "ContDataQC" / "historic_temperature_project"
UPLOAD_DIR = BASE_DIR / "TemperatureDB" / "db"
FIRST_R_SCRIPT = CONT_DIR / "csv_qc2.R"
SECOND_R_SCRIPT = CONT_DIR / "migration_prep2.R"
PYTHON_SCRIPT = BASE_DIR / "insert_temp_results_cross_platform.py"

def run_r_script(script_path):
    print(f"Running R script: {script_path}")
    subprocess.run(["Rscript", str(script_path)], check=True)

def run_python_script(script_path):
    print(f"Running Python script: {script_path}")
    subprocess.run([sys.executable, str(script_path)], check=True)

def main():
    run_r_script(FIRST_R_SCRIPT)

    run_extra = True  # Set this to False if you don't want to run the extra scripts
    if run_extra:
        run_r_script(SECOND_R_SCRIPT)
        run_python_script(PYTHON_SCRIPT)

if __name__ == "__main__":
    main()