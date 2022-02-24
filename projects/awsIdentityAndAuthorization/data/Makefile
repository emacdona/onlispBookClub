
default: s3Data/

s3Data/: .deps_installed
	(. .venv/bin/activate \
	&& python csvToParquet.py)

.deps_installed: .venv/
	(. .venv/bin/activate \
	&& pip install pandas pyarrow parquet-tools \
	&& touch .deps_installed)

.venv/:
	python -m venv ./.venv

clean:
	rm -rf .venv *.parquet .deps_installed s3Data
