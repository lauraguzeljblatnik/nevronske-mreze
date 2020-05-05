#help function for diploma


def identiteta (n):
    import csv
    with open('identiteta.csv', 'w', newline='') as file:
        writer = csv.writer(file)
        for i in range (1,n+1):
            writer.writerow([1/i,1/i])

        
#identiteta(10000)


import pandas as pd

data = pd.read_csv('day.csv')[:600]

print(data.mean())
print(data.std())
