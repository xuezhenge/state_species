import pandas as pd
import csv
from geopy.geocoders import Nominatim
from geopy.exc import GeocoderTimedOut

geolocator = Nominatim(user_agent="myGeocoder")

def get_coordinates(county_name, state_name, country_name="USA"):
    location = None
    county_name = county_name.title()
    state_name = state_name.title()

    if "County" not in county_name:
        county_name += " County"

    try:
        geolocator = Nominatim(user_agent="specify_your_app_name_here")
        location = geolocator.geocode(f"{county_name}, {state_name}, {country_name}")
    except GeocoderTimedOut:
        return None

    if location:
        return location.latitude, location.longitude
    else:
        return None

input_file = "../inputs/counties_oranges/counties3.csv"  # Replace with your actual input CSV file path
output_file = "../inputs/counties_oranges/county_coordinates4.csv"

# Read the input CSV file using pandas
df = pd.read_csv(input_file)

with open(output_file, 'w', newline='') as csvfile:
    csv_writer = csv.writer(csvfile)
    csv_writer.writerow(["County", "State", "Latitude", "Longitude"])

    for index, row in df.iterrows():
        county = row["County"]
        state = row["State"]
        coords = get_coordinates(county, state)
        if coords:
            lat, lon = coords
            csv_writer.writerow([county, state, lat, lon])
        else:
            print(f"{index+2}: Coordinates not found for {county}, {state}")
