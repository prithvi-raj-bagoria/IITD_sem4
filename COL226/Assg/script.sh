#!/bin/bash
# filepath: /home/prithviraj/Programming/IITD_sem4/COL226/Assg/a3/script.sh

# Check if input pattern is provided
if [ $# -ne 1 ]; then
    echo "Usage: $0 <file_pattern>"
    echo "Example: $0 '*.txt' or $0 't*.txt'"
    exit 1
fi

pattern=$1
count=0

# Get current directory path
current_dir=$(pwd)

# Process each matching file in current directory
for input_file in $pattern; do
    if [ -f "$input_file" ]; then
        echo "Processing: $input_file"
        
        # Step 1: Join lines (remove newlines between dimensions and bracket)
        # Handle both matrix format (with [[) and vector format (with [)
        sed -i -E ':a;N;$!ba;s/([0-9]+,?[0-9]*)\s*\n\s*(\[\[)/\1 \2/g' "$input_file"
        sed -i -E ':a;N;$!ba;s/([0-9]+)\s*\n\s*(\[)/\1 \2/g' "$input_file"

        # Step 2: Ensure exactly one space between dimensions and bracket
        # Handle matrix format
        sed -i -E 's/([0-9]+,?[0-9]*)\s*(\[\[)/\1 \2/g' "$input_file"
        sed -i -E 's/([0-9]+,?[0-9]*)(\[\[)/\1 \2/g' "$input_file"

        # Handle vector format
        sed -i -E 's/([0-9]+)\s*(\[)/\1 \2/g' "$input_file" 
        sed -i -E 's/([0-9]+)(\[)/\1 \2/g' "$input_file"

        # Step 3: Remove all spaces between values in the lists
        sed -i -E 's/,\s+/,/g' "$input_file" # Remove spaces after commas
        sed -i -E 's/\[\s+/\[/g' "$input_file" # Remove spaces after [
        sed -i -E 's/\s+\]/\]/g' "$input_file" # Remove spaces before ]
        
        count=$((count+1))
    fi
done

echo "Files processed: $count"

# If no files were found/processed
if [ $count -eq 0 ]; then
    echo "No files matched the pattern: $pattern"
    echo "Current directory: $current_dir"
    exit 1
fi