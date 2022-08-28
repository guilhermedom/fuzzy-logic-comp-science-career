library(sets)

# All variables range between 0 and 100.
sets_options("universe", seq(0, 100, 1))

# Create namespace for the functions in this script.
util = new.env()

# Variables are set like this according to what the author believes is more
# related to computer science.
setVariables = function() {
    variables = set(
        mathAffinity = fuzzy_partition(
            varnames = c(mathMin = 5, mathMinor = 15, mathMedian = 50,
                         mathMajor = 75, mathMax = 90),
            sd = 10
        ),
        interpersonalAffinity = fuzzy_partition(
            varnames = c(interMin = 30, interMinor = 35, interMedian = 55,
                         interMajor = 75, interMax = 85),
            sd = 10
        ),
        codeAffinity = fuzzy_partition(
            varnames = c(codeMin = 10, codeMinor = 25, codeMedian = 50,
                         codeMajor = 75, codeMax = 95),
            sd = 10
        ),
        leadAffinity = fuzzy_partition(
            varnames = c(leadMin = 30, leadMinor = 50, leadMedian = 70,
                         leadMajor = 90, leadMax = 95),
            sd = 10
        ),
        studyAffinity = fuzzy_partition(
            varnames = c(studyMin = 20, studyMinor = 40, studyMedian = 60,
                         studyMajor = 80, studyMax = 90),
            sd = 10
        ),
        commAffinity = fuzzy_partition(
            varnames = c(commMin = 40, commMinor = 50, commMedian = 60,
                         commMajor = 70, commMax = 75),
            sd = 10
        ),
        class = fuzzy_partition(
            varnames = c(poor = 10, average = 50, good = 75, excellent = 95),
            sd = 10
        )
    )
    
    variables
}

# Following rules were defined according to what the author believes is more
# related to computer science.
setRules = function() {
    rules = set(
        fuzzy_rule(mathAffinity %is% mathMax
                   && interpersonalAffinity %is% interMin
                   && codeAffinity %is% codeMax
                   && leadAffinity %is% leadMin
                   && studyAffinity %is% studyMax
                   && commAffinity %is% commMin,
                   class %is% excellent),
        fuzzy_rule(mathAffinity %is% mathMax
                   && interpersonalAffinity %is% interMinor
                   && codeAffinity %is% codeMajor
                   && leadAffinity %is% leadMin
                   && studyAffinity %is% studyMax
                   && commAffinity %is% commMinor,
                   class %is% excellent
        ),
        fuzzy_rule(mathAffinity %is% mathMajor
                   && interpersonalAffinity %is% interMinor
                   && codeAffinity %is% codeMajor
                   && leadAffinity %is% leadMin
                   && studyAffinity %is% studyMax
                   && commAffinity %is% commMedian,
                   class %is% excellent
        ),
        fuzzy_rule(mathAffinity %is% mathMajor
                   && interpersonalAffinity %is% interMinor
                   && codeAffinity %is% codeMajor
                   && leadAffinity %is% leadMinor
                   && studyAffinity %is% studyMajor
                   && commAffinity %is% commMedian,
                   class %is% good
        ),
        fuzzy_rule(mathAffinity %is% mathMajor
                   && interpersonalAffinity %is% interMedian
                   && codeAffinity %is% codeMedian
                   && leadAffinity %is% leadMedian
                   && studyAffinity %is% studyMajor
                   && commAffinity %is% commMajor,
                   class %is% good
        ),
        fuzzy_rule(mathAffinity %is% mathMajor
                   && interpersonalAffinity %is% interMedian
                   && codeAffinity %is% codeMedian
                   && leadAffinity %is% leadMajor
                   && studyAffinity %is% studyMajor
                   && commAffinity %is% commMajor,
                   class %is% good
        ),
        fuzzy_rule(mathAffinity %is% mathMedian
                   && interpersonalAffinity %is% interMajor
                   && codeAffinity %is% codeMinor
                   && leadAffinity %is% leadMajor
                   && studyAffinity %is% studyMedian
                   && commAffinity %is% commMajor,
                   class %is% average
        ),
        fuzzy_rule(mathAffinity %is% mathMedian
                   && interpersonalAffinity %is% interMajor
                   && codeAffinity %is% codeMinor
                   && leadAffinity %is% leadMajor
                   && studyAffinity %is% studyMedian
                   && commAffinity %is% commMax,
                   class %is% average
        ),
        fuzzy_rule(mathAffinity %is% mathMinor
                   && interpersonalAffinity %is% interMax
                   && codeAffinity %is% codeMin
                   && leadAffinity %is% leadMax
                   && studyAffinity %is% studyMedian
                   && commAffinity %is% commMax,
                   class %is% average
        ),
        fuzzy_rule(mathAffinity %is% mathMinor
                   && interpersonalAffinity %is% interMax
                   && codeAffinity %is% codeMin
                   && leadAffinity %is% leadMax
                   && studyAffinity %is% studyMinor
                   && commAffinity %is% commMax,
                   class %is% poor
        ),
        fuzzy_rule(mathAffinity %is% mathMin
                   && interpersonalAffinity %is% interMax
                   && codeAffinity %is% codeMin
                   && leadAffinity %is% leadMax
                   && studyAffinity %is% studyMinor
                   && commAffinity %is% commMax,
                   class %is% poor
        ),
        fuzzy_rule(mathAffinity %is% mathMin
                   && interpersonalAffinity %is% interMax
                   && codeAffinity %is% codeMin
                   && leadAffinity %is% leadMax
                   && studyAffinity %is% studyMin
                   && commAffinity %is% commMax,
                   class %is% poor
        )
    )
    
    rules
}

# Export function to other scripts that load this script.
util$setFuzzySystem = function() {
    system = fuzzy_system(setVariables(), setRules())
    
    system
}