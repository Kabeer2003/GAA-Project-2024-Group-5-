import pandas as pd
import requests

# Function to get geocode from Google Maps API
def get_geocode(postal_code, api_key):
    base_url = "https://maps.googleapis.com/maps/api/geocode/json"
    params = {
        "address": postal_code,
        "key": api_key
    }
    response = requests.get(base_url, params=params)
    
    if response.status_code == 200:
        geocode_data = response.json()
        if geocode_data['status'] == 'OK':
            lat = geocode_data['results'][0]['geometry']['location']['lat']
            lng = geocode_data['results'][0]['geometry']['location']['lng']
            return (lat, lng)
        else:
            return (None, None)
    else:
        raise Exception("Response status code: " + str(response.status_code))

# Replace 'your_api_key' with your actual Google Maps API key
api_key = 'AIzaSyA9CWXEmMox7tEkRgITSDm0vFeEoPNi3KU'

# Load the data from CSV
csv_path = 'c:\\Users\\91770\\Downloads\\hawker_centres.csv'
df = pd.read_csv(csv_path)

# Create columns for latitude and longitude
df['Latitude'] = None
df['Longitude'] = None

# Iterate through the DataFrame and get geocodes for each postal code
for index, row in df.iterrows():
    # Remove the leading 'S' from the postal code
    clean_postal_code = row['Postal Code']
    lat_lng = get_geocode(clean_postal_code, api_key)
    df.at[index, 'Latitude'] = lat_lng[0]
    df.at[index, 'Longitude'] = lat_lng[1]

# Save the updated DataFrame back to CSV
df.to_csv('updated_hawker_centres.csv', index=False)
