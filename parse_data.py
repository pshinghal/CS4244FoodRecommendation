import sys

Fields = {
	'Name': 0,
	'isVegetarian': 1,
	'Cuisine': 2,
	'isSweet': 3,
	'isSour': 4,
	'isSpicy': 5,
	'isLowCalory': 6,
	'isLowSodium': 7,
	'isLowFat': 8,
	'isHighFiber': 9
}
def makeFact(line):
	facts = []
	facts.append("name " + line[Fields['Name']])
	facts.append("cuisine " + line[Fields['Cuisine']])
	if line[Fields['isVegetarian']] == 'TRUE':
		facts.append("vegetarian")

	if line[Fields['isSweet']] == 'TRUE':
		facts.append("sweet")
	if line[Fields['isSour']] == 'TRUE':
		facts.append("sour")
	if line[Fields['isSpicy']] == 'FALSE':
		facts.append("spiciness 0")
	elif line[Fields['isSpicy']] == 'True 1':
		facts.append("spiciness 1")
	elif line[Fields['isSpicy']] == 'True 2':
		facts.append("spiciness 2")
	elif line[Fields['isSpicy']] == 'True 3':
		facts.append("spiciness 3")
	elif line[Fields['isSpicy']] == 'True 4':
		facts.append("spiciness 4")
	else:
		print "ERROR: invalid spiciness value!", line[Fields['isSpicy']]
		exit(0)

	if line[Fields['isLowCalory']] == 'TRUE':
		facts.append("lowcal")
	if line[Fields['isLowSodium']] == 'TRUE':
		facts.append("lowna")
	if line[Fields['isLowFat']] == 'TRUE':
		facts.append("lowfat")
	if line[Fields['isHighFiber']] == 'TRUE':
		facts.append("highfiber")

	return "(dish (" + ") (".join(facts) + "))"

# discard first line
raw_input()
for line in sys.stdin:
	print makeFact(line.split(','))
