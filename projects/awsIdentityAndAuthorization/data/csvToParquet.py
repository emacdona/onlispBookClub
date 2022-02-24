import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq
import os
import re

data_dir = "s3Data/bucket-%03d/%05d/%s_%05d"

for i in range(0,10):
    for f in os.listdir():
        if(f.endswith(".csv")):
            csv_file = f
            base_name = os.path.splitext(csv_file)[0]
            base_dir = data_dir % (i % 5, i, base_name, i)
            parquet_file = "%s/%05d_%s.parquet" % (base_dir, i, base_name)
            os.makedirs(base_dir, exist_ok=True)

            df = pd.read_csv(csv_file)
            df["DATASET_ID"] = i

            # Guess the schema of the CSV file from the first chunk
            parquet_schema = pa.Table.from_pandas(df=df).schema

            # Open a Parquet file for writing
            parquet_writer = pq.ParquetWriter(parquet_file, parquet_schema, compression='snappy')

            # Write CSV chunk to the parquet file
            table = pa.Table.from_pandas(df, schema=parquet_schema)
            parquet_writer.write_table(table)

            parquet_writer.close()
