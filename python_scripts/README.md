# WHO-BCN Data Extraction Scripts

## Installation

To install the dependencies required to run this script, use the `requirements.txt` file:

```bash
pip install -r requirements.txt
```

If you prefer to use a virtual environment, you can create one and activate it before installing the dependencies:

```bash
python3 -m venv env
source env/bin/activate
pip install -r requirements.txt
```

## make_quantitative_bulk_load_file.py

This script processes CSV files from the "Data Extraction Tool" into "Bulk Load" XLSX files.
The script needs a Bulk Load template. It can either be supplied with the `--xlsx_template` argument or by placing a template named "Quantitative_Data_UHCPW_Template.xlsx" in the same folder as the script.
The output file will be named as the input CSV file, but with XLSX extension.

### Usage

```
python3 make_quantitative_bulk_load_file.py [-h | --help] [-x | --xlsx_template <XLSX_TEMPLATE>] [-r | --real_value] [-d | --debug] indicators_csv
```

### Examples

Simple use:
```bash
python3 make_quantitative_bulk_load_file.py SPA_fp_indicators.csv
```

Specifying a template and using the 'real_value' column from the source file:
```bash
python3 make_quantitative_bulk_load_file.py SPA_fp_indicators.csv -r --xlsx_template=~/docs/Quantitative_Template.xlsx
```

Using debug flag and redirecting the debug text into a file:
```bash
python3 make_quantitative_bulk_load_file.py SPA_fp_indicators.csv -d > script_log.json
```

### Positional Arguments

`indicators_csv`: The source CSV file.

### Options

`-h`, `--help`: Show the help message and exit.

`-x XLSX_TEMPLATE`, `--xlsx_template XLSX_TEMPLATE`: The Bulk Load Quantitative XLSX template file path. If empty, the script will try to open "Quantitative_Data_UHCPW_Template.xlsx".

`-r`, `--real_value`: Use `real_value` instead of `value` from the CSV source file.

`-d`, `--debug`: Display debug logs. It is recommended to redirect the output into a file, e.g: `... > log.json`.


## make_qualitative_bulk_load_file.py

This script processes DOCX files into "Bulk Load" XLSX files.
The script needs a Bulk Load template. It can either be supplied with the `--xlsx_template` argument or by placing a template named "Quantitative_Data_UHCPW_Template.xlsx" in the same folder as the script.
The output file will be a XLSX file named _\<COUNTRY>\_\<YEAR>\_Qualitative_Data.xlsx_.

### Usage

```
python3 make_qualitative_bulk_load_file.py [-h] [-x XLSX_TEMPLATE] [-d] docx_filename
```

### Examples

Simple use:
```bash
python3 make_qualitative_bulk_load_file.py summary_tables.docx
```

Specifying a template, using debug flag and redirecting the debug text into a file:
```bash
python3 make_qualitative_bulk_load_file.py summary_tables.docx --xlsx_template=~/docs/Qualitative_Template.xlsx -d > script_log.json
```

### Positional Arguments

`docx_filename`: The path to the DOCX source file.

### Options

`-h`, `--help`: Show the help message and exit.

`-x XLSX_TEMPLATE`, `--xlsx_template XLSX_TEMPLATE`: The Bulk Load Quantitative XLSX template file path. If empty, the script will try to open "Quantitative_Data_UHCPW_Template.xlsx".

`-d`, `--debug`: Display debug logs. It is recommended to redirect the output into a file, e.g: `... > log.json`.