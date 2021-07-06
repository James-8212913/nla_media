import requests
import json as js
import pandas as pd

# API Headers

headers = {'Authorization': '0e4167a6dea241259827a0aba74ab876'}

# API Endpoints

everything_url = 'https://newsapi.org/v2/everything'
top_headlines_url = 'https://newsapi.org/v2/everything'
sources_rul = 'https://newsapi.org/v2/sources'

headlines_payload = {'category': 'world', 'country': 'australia'}
everything_payload = {'q': 'china', 'language': 'en', 'sortBy': 'popularity'}
sources_payload = {'category': 'general'}

response = requests.get(url=everything_url, headers = headers, params = everything_payload)

pretty_json_output = js.dumps(response.json(), indent = 4)
#print(pretty_json_output)

response_json_string = js.dumps(response.json())
#print(response_json_string)

response_dict = js.loads(response_json_string)

articles_list = response_dict['articles']

df1 = pd.read_json(js.dumps(articles_list))

#df1.to_csv('/Users/james/Desktop/Data_Science/nla_media_security/data/news.csv')
