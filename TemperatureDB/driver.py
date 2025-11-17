import subprocess
import sys
from pathlib import Path

#MAKE SURE YOU PUT YOUR USERNAME AND PASSWORD IN user.cnf.txt
#ex.
#user,YourUsername
#password,YourPassword

# File structure required for this program
'''
home
└── ContDataQC                                        #https://github.com/ctdeepwatermonitoring/water_temperature
    ├── historic_temperature_project
    │   ├── csv_qc2.R
    │   ├── migration_prep2.R
    │   ├── config_deep2.R
    │   ├── data_to_qc
    │   ├── qced_data
    │   ├── raw_data                                  #DEPLOYMENT CSVS GO HERE
    │   ├── error_files
    │   │   └── error_log
    │   └── migration_folder
    └── TemperatureDB
        └── db
            ├── cnf
            │   └── user.cnf.txt
            ├── testFTP
            │   └── Upload
            │       ├── ContData
            │       ├── ErrRpts
            │       └── UploadedRpts
            │           └── Temperature
            │               └── Error
            └── db_app
                ├── driver.py
                └── insert_temp_results_cross_platform.py
'''

BASE_DIR = Path.home()
CONT_DIR = BASE_DIR / "ContDataQC" / "historic_temperature_project"
UPLOAD_DIR = BASE_DIR / "ContDataQC" / "TemperatureDB" / "db" / "db_app"
QC_SCRIPT = CONT_DIR / "csv_qc2.R"
UPLOAD_PREP_SCRIPT = CONT_DIR / "migration_prep2.R"
UPLOAD_SCRIPT = UPLOAD_DIR / "insert_temp_results_cross_platform.py"

def run_r_script(script_path):
    print(f"Running R script: {script_path}")
    subprocess.run(["Rscript", str(script_path)], check=True)

def run_python_script(script_path):
    print(f"Running Python script: {script_path}")
    script_module = "db.db_app.insert_temp_results_cross_platform"
    subprocess.run([sys.executable, "-m", script_module], check=True)

def main():
    run_qc = True # Set this to False if you don't want to do the qc
    if run_qc:
        run_r_script(QC_SCRIPT)

    run_upload = True  # Set this to False if you don't want to upload to the database
    if run_upload:
        run_r_script(UPLOAD_PREP_SCRIPT)
        run_python_script(UPLOAD_SCRIPT)

if __name__ == "__main__":
    main()