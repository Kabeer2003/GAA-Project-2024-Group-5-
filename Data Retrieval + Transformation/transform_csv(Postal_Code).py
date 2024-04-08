import pandas as pd

# Load the DataFrame (assuming you've already converted your PDF to CSV)
df = pd.read_csv('hawker_stalls.csv', dtype=str)  # Make sure all data is read as strings

# Initialize a variable to store the current postal code
current_postal_code = None

# Iterate over the DataFrame rows
for index, row in df.iterrows():
    # Check if the current row's postal code field is not empty
    if pd.notna(row['PostalCode']) and row['PostalCode'].startswith('S'):
        # Update the current postal code
        current_postal_code = row['PostalCode']
    elif pd.isna(row['PostalCode']) or row['PostalCode'].isspace():
        # If the line is blank (separator), reset the current postal code
        current_postal_code = None
    else:
        # Fill in the current postal code for the current row if it's part of a sublist
        if current_postal_code is not None:
            df.at[index, 'PostalCode'] = current_postal_code

# Save the updated DataFrame back to CSV
df.to_csv('hawker_stalls_updated.csv', index=False)
