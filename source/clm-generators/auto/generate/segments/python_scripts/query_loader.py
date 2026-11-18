from pathlib import Path
from connection import get_ch_client

def load_query(query_path):
    with open(query_path,'r') as file:
        return file.read()

def execute_query(query):
    connect = get_ch_client()
    connect.command(query)

def fetch_results(query):
    connect = get_ch_client()
    connect.command(query)
    return connect.query(query).result_rows

