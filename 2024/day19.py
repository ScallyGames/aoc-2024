from functools import cache

file = open("inputs/input19.txt", "r")
content = file.read()
file.close()

def getTowelCombinationCount(pattern, towels):
    def getValidCandidates(nextPattern):
        return [towel for towel in towels if towel == nextPattern[0:len(towel)]]

    @cache
    def getTowelCombinationCountRecursive(remainingPattern):
        if len(remainingPattern) == 0:
            return 1

        possibleCandidates = getValidCandidates(remainingPattern)
        candidateLengths = [getTowelCombinationCountRecursive(remainingPattern[len(x):]) for x in possibleCandidates]
        combinationCount = sum(candidateLengths)
        return combinationCount

    return getTowelCombinationCountRecursive(pattern)

(towelString, patternsInput) = content.strip().split("\n\n")
towelOptions = [x.strip() for x in towelString.split(",")]


patterns = patternsInput.split("\n")

possibleDesigns = [getTowelCombinationCount(x, towelOptions) for x in patterns]

print(sum(possibleDesigns))
