import re


file = open("inputs/input03.txt", "r")
content = file.read()
file.close()

matches = re.findall(r'mul\((\d{1,3}),(\d{1,3})\)', content)

multiplicationResults = [int(a) * int(b) for (a, b) in matches]

print(f"Part 1: {sum(multiplicationResults)}")


# iterate through characters 1 at a time and keep state
# regex finding groups of do and don't - how to handle start and end
# multiplicationResults store index where it happened and conditionally sum depending on indexes of do and don't



currentPointer = 0

mulPattern = re.compile(r'mul\((\d{1,3}),(\d{1,3})\)')

isActive = True

result = 0

numberOfSteps = 0


while currentPointer < len(content):
    mulFindResult = mulPattern.search(content, currentPointer)
    nextMul = mulFindResult.start() if mulFindResult else len(content)
    nextDo = content.find('do()', currentPointer)
    nextDo = nextDo if nextDo > 0 else len(content)
    nextDont = content.find('don\'t()', currentPointer)
    nextDont = nextDont if nextDont > 0 else len(content)
    
    nextIndex = min(nextMul, nextDo, nextDont)

    if nextIndex == len(content): 
        break

    if nextIndex == nextMul:
        if isActive:
            mulMatch = mulPattern.match(content, nextIndex)
            result += int(mulMatch.group(1)) * int(mulMatch.group(2))
    elif nextIndex == nextDo:
        isActive = True
    elif nextIndex == nextDont:
        isActive = False

    currentPointer = nextIndex + 1

    numberOfSteps += 1

print(f'Part 2: {result}')