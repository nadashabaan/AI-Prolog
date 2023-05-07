import tkinter as tk
import os
from pyswip import Prolog

#'C:\\Program Files\\swipl\\bin'
swipath ='C:\Program Files\swipl'
os.environ['PATH'] = swipath + os.pathsep + os.environ['PATH']


prolog = Prolog()
prolog.consult("informedsearch.pl")
# Create a new window
root = tk.Tk()
root.title(" Informed Search Algorithm")
root.size()
root.geometry("800x600")
label1 = tk.Label(root, text="Enter Your Query:")
# Create the input field
input_field = tk.Entry(root)
# Create two lists to store the output
list1 = tk.Listbox(root)


# Function to be called when the "Button 1" is clicked
def input():
    input_value = input_field.get()
    result = list(prolog.query(input_value))
    for item in result:
        list1.insert(tk.END, item)


button1 = tk.Button(root, text="print the result", command=input,fg="blue")
label1.pack(padx=10, fill=tk.BOTH)
input_field.pack(padx=10, pady=10, fill=tk.BOTH)
button1.pack()
list1.pack(padx=10, pady=10, fill=tk.BOTH, expand=True)


root.mainloop()