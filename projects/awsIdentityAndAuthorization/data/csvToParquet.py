# csv_to_parquet.py

import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq
import os
import re

parquet_file = '/path/to/my.parquet'
chunksize = 100_000

for i in range(0,100):
    for f in os.listdir():
        if(f.endswith(".csv")):
            csv_file = f
            parquet_file = re.sub('\.csv', '.parquet', f)
            csv_stream = pd.read_csv(csv_file, sep=',', chunksize=chunksize, low_memory=False)
            csv_stream["DATASET_ID"] = i

            for i, chunk in enumerate(csv_stream):
                print("Chunk", i)
                if i == 0:
                    # Guess the schema of the CSV file from the first chunk
                    parquet_schema = pa.Table.from_pandas(df=chunk).schema
                    # Open a Parquet file for writing
                    parquet_writer = pq.ParquetWriter(parquet_file, parquet_schema, compression='snappy')

                # Write CSV chunk to the parquet file
                table = pa.Table.from_pandas(chunk, schema=parquet_schema)
                parquet_writer.write_table(table)

            parquet_writer.close()
