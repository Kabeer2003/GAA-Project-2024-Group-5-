import pandas as pd
import PyPDF2

def process_pdf_to_csv(pdf_path, csv_path):
    with open(pdf_path, 'rb') as pdf_file:
        pdf_reader = PyPDF2.PdfReader(pdf_file)
        num_pages = len(pdf_reader.pages)
        text = ""
        
        for page_num in range(num_pages):
            page = pdf_reader.pages[page_num]
            text += page.extract_text() + "\n"
        
    # Manual text processing with additional validation
    rows = text.split("\n")
    # Filtering out rows that contain the pattern 'Back to XX'
    # This assumes that the unwanted text does not contain commas that would split it across multiple list items
    filtered_rows = [row for row in rows if not row.startswith('Back to')]
    data = [row.split(",") for row in filtered_rows if row]
    
    df = pd.DataFrame(data)
    
    # Save the cleaned DataFrame to CSV
    df.to_csv(csv_path, index=False)

pdf_paths = [
    ('c:\\wamp64\\www\\Datathon stuff\\data\\dbs-paylah-hawker-centers-list.pdf', 'hawker_centres.csv'),
    ('c:\\wamp64\\www\\Datathon stuff\\data\\dbs-paylah-hawker-stalls-list.pdf', 'hawker_stalls.csv')
]

for pdf_path, csv_path in pdf_paths:
    process_pdf_to_csv(pdf_path, csv_path)
