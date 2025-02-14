import sys
import base64
import json
from googleapiclient.discovery import build

def google_search(query, api_key, cse_id, num_results=5):
    """
    Query Google Custom Search API and return a list of search results.
    
    Parameters:
      query (str): The search query.
      api_key (str): Your Google API key.
      cse_id (str): Your Custom Search Engine ID.
      num_results (int): Number of results to return.
    
    Returns:
      list: A list of dictionaries containing search result data.
    """
    service = build("customsearch", "v1", developerKey=api_key)
    res = service.cse().list(q=query, cx=cse_id, num=num_results).execute()
    results = res.get('items', [])

    for item in results:
        print("Title:", item.get("title"))
        print("Link: ", item.get("link"))
        print("Snippet:", item.get("snippet"))
        print("-" * 10)

def main():
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <base64_encoded_json>")
        sys.exit(1)
    
    base64_str = sys.argv[1]
    
    try:
        # Decode base64
        decoded_bytes = base64.b64decode(base64_str)
        # Convert to UTF-8 string
        json_str = decoded_bytes.decode('utf-8')
        # Parse JSON
        data = json.loads(json_str)
        
        # Extract fields
        search_query = data['searchQuery']
        search_number = data['searchNum']
        search_apikey = data['searchApiKey']
        search_cx = data['searchCseId']
        
        # Call search function with extracted parameters
        google_search(search_query, search_apikey, search_cx, search_number)
    
    except base64.binascii.Error as e:
        print(f"Base64 decoding error: {e}")
        sys.exit(1)
    except UnicodeDecodeError as e:
        print(f"UTF-8 decoding error: {e}")
        sys.exit(1)
    except json.JSONDecodeError as e:
        print(f"JSON parsing error: {e}")
        sys.exit(1)
    except KeyError as e:
        print(f"Missing required field in JSON: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"Unexpected error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
