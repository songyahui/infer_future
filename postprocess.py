import csv

# Input and output file paths
input_file = 'infer-future/report.csv'
output_file = 'infer-future/reportPost.csv'

# Define headers
headers = ['Category', 'LoC', 'PrimS', 'InferredS', 'InferredInv', 'Report', 'Time(s)']

# Read the input data
data = []
with open(input_file, 'r') as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
        # Split the path to get just the filename as category
        category = row[0].split('/')[-1]  # Takes the last part of the path
        data.append([category] + row[1:])

# Calculate sums for the summary row
sum_loc = sum(int(row[1]) for row in data)
# sum_prims = sum(int(row[2]) for row in data)
sum_inferreds = sum(int(row[3]) for row in data)
sum_inferredinv = sum(int(row[4]) for row in data)
sum_report = sum(int(row[5]) for row in data)
sum_time = sum(float(row[6]) for row in data)

# Create the summary row
summary_row = ['-', str(sum_loc), '-', str(sum_inferreds), 
               str(sum_inferredinv), str(sum_report), f"{sum_time:.9f}"]

# Write to output file with headers and summary
with open(output_file, 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    
    # Write headers
    writer.writerow(headers)
    
    # Write original data (with full paths)
    with open(input_file, 'r') as infile:
        for line in infile:
            writer.writerow(line.strip().split(','))
    
    # Write summary row
    writer.writerow(summary_row)

print(f"Processed CSV file has been saved to {output_file}")


def pretty_print_csv(filename):
    with open(filename, 'r') as file:
        reader = csv.reader(file)
        data = list(reader)
    
    # Calculate column widths
    col_widths = [max(len(str(item)) for item in col) for col in zip(*data)]
    
    # Print header separator
    print('+' + '+'.join(['-' * (width + 2) for width in col_widths]) + '+')
    
    # Print rows
    for i, row in enumerate(data):
        # Print header row differently
        if i == 0:
            print('| ' + ' | '.join(f"{str(item):^{width}}" for item, width in zip(row, col_widths)) + ' |')
            print('+' + '+'.join(['=' * (width + 2) for width in col_widths]) + '+')
        else:
            print('| ' + ' | '.join(f"{str(item):<{width}}" for item, width in zip(row, col_widths)) + ' |')
            print('+' + '+'.join(['-' * (width + 2) for width in col_widths]) + '+')

pretty_print_csv(output_file)