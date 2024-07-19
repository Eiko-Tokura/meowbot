import os
import argparse
from pdf2image import convert_from_path

def pdf_to_png(pdf_path, output_folder):
    # Convert PDF to list of images
    images = convert_from_path(pdf_path)

    # Make sure the output folder exists
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)

    # Save each image as a PNG file
    for i, image in enumerate(images):
        image_path = os.path.join(output_folder, f'page_{i + 1}.png')
        image.save(image_path, 'PNG')
        print(f'Saved {image_path}')

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Convert PDF to PNG images.')
    parser.add_argument('pdf_path', type=str, help='Path to the PDF file.')
    parser.add_argument('output_folder', type=str, help='Folder to save PNG images.')

    args = parser.parse_args()
    
    pdf_to_png(args.pdf_path, args.output_folder)
