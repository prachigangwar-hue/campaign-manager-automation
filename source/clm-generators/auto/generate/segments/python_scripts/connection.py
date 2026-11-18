import time
import clickhouse_connect
from clickhouse_connect.driver.exceptions import OperationalError

MAX_RETRIES = 5          # Maximum number of retries
BASE_DELAY = 2           # Base delay for exponential backoff (in seconds)
MAX_DELAY = 60  

# developement server
CH_HOST_DEV = "15.206.249.145"
CH_PORT_DEV = "8123"
CH_USERNAME_DEV = "savyasachi"
CH_PASSWORD_DEV = "REDACTED_REQUIRED"
CH_DATABASE_DEV = "vmart_sales"

# production server
CH_HOST = "chn1.vmart-tools.com"
CH_PORT = "8123"
CH_USERNAME = "analyst_user"
CH_PASSWORD = "REDACTED_REQUIRED"
CH_DATABASE = "temp_db"

def get_ch_client_dev():
    client = clickhouse_connect.get_client(
        host=CH_HOST_DEV,
        port=int(CH_PORT_DEV),
        username=CH_USERNAME_DEV,
        password=CH_PASSWORD_DEV,
        database=CH_DATABASE_DEV,
        send_receive_timeout=120 
    )
    return client

def get_ch_client():
    attempt = 0
    while attempt < MAX_RETRIES:
        try:
            print(f'Attempt {attempt + 1} to connect to ClickHouse...')
            client = clickhouse_connect.get_client(
                host=CH_HOST,
                port=CH_PORT,
                username=CH_USERNAME,
                password=CH_PASSWORD,
                database=CH_DATABASE,
                send_receive_timeout=300,
                connect_timeout=60
            )
            print('Connection established successfully!')
            return client
        except OperationalError as e:
            print(f'Connection failed: {e}')
            attempt += 1
            wait_time = min(BASE_DELAY * (2 ** (attempt - 1)), MAX_DELAY)
            print(f'Retrying in {wait_time} seconds...')
            time.sleep(wait_time)

    print(f'Failed to connect to ClickHouse after {MAX_RETRIES} attempts.')
    raise ConnectionError('Could not establish connection to ClickHouse after multiple attempts.')
