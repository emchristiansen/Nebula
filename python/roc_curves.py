from collections import *
import os
from pylab import *

homeDirectory = os.getenv("HOME")

resultsDirectory = os.path.join(homeDirectory, "Dropbox/head_segmentation/SFSPipeline/results")
rocDataDirectory = os.path.join(resultsDirectory, "roc_data")
rocCurveDirectory = os.path.join(resultsDirectory, "roc_curves")

rocDataFiles = filter(lambda f: ".txt" in f, os.listdir(rocDataDirectory))

def addROCCurve(f):
    meanROCLines = open(os.path.join(rocDataDirectory, f)).readlines()[-3:]
    assert("Mean" in meanROCLines[0])

    toFloat = lambda s: float(s)

    fprs = map(toFloat, meanROCLines[1].split())
    tprs = map(toFloat, meanROCLines[2].split())

    plot(fprs, tprs, label=f.replace(".txt", ""))

def wrapString(s):
    maxLen = 80
    if len(s) <= maxLen:
        return s
    else:
        return s[:maxLen] + "\n" + wrapString(s[maxLen:])

def rocFigure(title, rocFilenames):
    figure()
    suptitle(wrapString(title))
    xlabel("false positive rate")
    ylabel("true positive rate")
    axis([0, 1, 0, 1])
    
    for f in rocFilenames: addROCCurve(f)

    legend(loc='lower right')

    setp(gca().get_legend().get_texts(), fontsize='small')

    rocFilename = title.replace(" ", "_") + "_mean_roc.png"
    rocPath = os.path.join(rocCurveDirectory, rocFilename)

    print "writing %s" % (rocPath)
    savefig(rocPath)
    
Experiment = namedtuple('Experiment', ['time', 'roi', 'distance', 'pose', 'illumination', 'blur', 'noise', 'jpeg', 'misalignment', 'background'])

def parseFilename(f):
    fn = os.path.splitext(os.path.split(f)[1])[0]
    vals = fn.split("_")
    return Experiment(*vals)

ExperimentSet = namedtuple('ExperimentSet', ['roi', 'distance', 'pose', 'illumination', 'blur', 'noise', 'jpeg', 'misalignment', 'background'])

backgroundSet = ExperimentSet("HER HNR TFR", "LBP", "051x051", "00x00", "0x0", "0x0", "0x0", "0x0", "falsexfalse truextrue")

blurLBPSet = ExperimentSet("HNR TFR", "LBP", "051x051", "00x00", "0x0 2x2 8x8", "0x0", "0x0", "0x0", "falsexfalse")

blurEigenSet = ExperimentSet("HNR TFR", "Eigenface", "051x051", "00x00", "0x0 2x2 8x8", "0x0", "0x0", "0x0", "falsexfalse")

backgroundLBPSet = ExperimentSet("HNR TFR", "LBP", "051x051", "00x00", "0x0", "0x0", "0x0", "0x0", "falsexfalse truextrue")

backgroundEigenSet = ExperimentSet("HNR TFR", "Eigenface", "051x051", "00x00", "0x0", "0x0", "0x0", "0x0", "falsexfalse truextrue")

jitterLBPSet = ExperimentSet("HNR TFR", "LBP", "051x051", "00x00", "0x0", "0x0", "0x0", "0x0 3x3 12x12", "falsexfalse")

jitterEigenSet = ExperimentSet("HNR TFR", "Eigenface", "051x051", "00x00", "0x0", "0x0", "0x0", "0x0 3x3 12x12", "falsexfalse")

saturationLBPSet = ExperimentSet("HNR TFR", "LBP", "051x051", "00x00 0.25xx0.25x", "0x0", "0x0", "0x0", "0x0", "falsexfalse")

saturationEigenSet = ExperimentSet("HNR TFR", "Eigenface", "051x051", "00x00 0.25xx0.25x", "0x0", "0x0", "0x0", "0x0", "falsexfalse")

#sets = [backgroundSet, blurLBPSet, blurEigenSet, backgroundLBPSet, backgroundEigenSet, jitterLBPSet, jitterEigenSet]
sets = [saturationLBPSet, saturationEigenSet]

profileSet = ExperimentSet("HER PR TFR", "LBP", "240x240", "00x00", "0 2 8", "0", "0", "0", "falsexfalse truextrue")

def experimentSetContains(s, e):
    return (e.roi in s.roi.split()) and (e.distance in s.distance.split()) and (e.pose in s.pose.split()) and (e.illumination in s.illumination.split()) and (e.blur in s.blur.split()) and (e.noise in s.noise.split()) and (e.jpeg in s.jpeg.split()) and (e.misalignment in s.misalignment.split()) and (e.background in s.background.split())

def processExperimentSet(experimentSet):
    matches = []
    alreadySeen = []
    for f in rocDataFiles:
        e = parseFilename(f)._replace(time="")
        if experimentSetContains(experimentSet, e) and e not in alreadySeen:
            print "adding %s to set %s" % (f, experimentSet)
            matches.append(f)
            alreadySeen.append(e)

    rocFigure(str(experimentSet), matches)

if __name__ == '__main__':
    for s in sets: processExperimentSet(s)

# differInBackground = []
# dBTitles = []
# for f1i in range(len(rocDataFiles)):
#     f1 = rocDataFiles[f1i]

#     if any([f1 in d for d in differInBackground]): 
# #        print "continuing"
# #        print [f1 in d for d in differInBackground]
#         continue

#     fs = [f1]
#     e1 = parseFilename(f1)
#     dBTitles.append(str(e1._replace(distance="", background="")))

#     for f2 in rocDataFiles[f1i + 1:]:
#         e2 = parseFilename(f2)

#         if e1._replace(background=e2.background, distance=e2.distance) == e2:
#             fs.append(f2)

#     differInBackground.append(fs)

# acrossROIs = []
# aRTitles = []
# for f1i in range(len(rocDataFiles)):
#     f1 = rocDataFiles[f1i]

#     if any([f1 in d for d in acrossROIs]): 
# #        print "continuing"
# #        print [f1 in d for d in differInBackground]
#         continue

#     fs = [f1]
#     e1 = parseFilename(f1)
#     aRTitles.append(str(e1._replace(roi="")))

#     for f2 in rocDataFiles[f1i + 1:]:
#         e2 = parseFilename(f2)

#         if e1._replace(roi=e2.roi) == e2:
#             fs.append(f2)

#     acrossROIs.append(fs)

# # for (t, fs) in zip(dBTitles, differInBackground):
# #     rocFigure(t, fs)

# # for (t, fs) in zip(aRTitles, acrossROIs):
# #     rocFigure(t, fs)




