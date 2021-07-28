import pandas as pd

trials = pd.read_csv("exp2_trials_usable.csv")
cols=trials.columns
ncol = cols.size
output = ""
for i in trials.index:
    trial = "{"
    for j in range(0,(ncol-1)):
        line = "\n\t" + cols[j]+": "+ str(trials[cols[j]][i]) + ","
        trial = trial + line
    trial_tail = "\n\t" +  cols[ncol-1]+": "+ str(trials[cols[ncol-1]][i]) + "\n},\n" #max line
    trial = trial + trial_tail
    output= output + trial
file = open("stimuli.js","w")  
file.write("exp.stims_cb = [ \n")
file.writelines(output)
file.write("];")
file.close()

# for ind in trials.index:
#     trial = ""
#     trial = "{ \n\t"+ cols[0]+": "+ str(trials[cols[0]][ind]) + ",\n\t" + cols[1]+": "+ str(trials[cols[1]][ind]) + ",\n\t" + cols[2]+": "+ str(trials[cols[2]][ind]) + ",\n\t" + cols[3]+": "+ str(trials[cols[3]][ind]) + ",\n\t" + cols[4]+": "+ str(trials[cols[4]][ind]) + ",\n\t" + cols[5]+": "+ str(trials[cols[5]][ind]) + ",\n\t" + cols[6]+": "+ str(trials[cols[6]][ind]) + ",\n\t" + cols[7]+": "+ str(trials[cols[7]][ind]) + ",\n\t" + cols[8]+": "+ str(trials[cols[8]][ind]) + ",\n\t" + cols[9]+": "+ str(trials[cols[9]][ind]) + ",\n\t" + cols[10]+": "+ str(trials[cols[10]][ind]) + ",\n\t" + cols[11]+": "+ str(trials[cols[11]][ind]) + ",\n\t" + cols[12]+": "+ str(trials[cols[12]][ind]) +",\n\t" +  cols[13]+": "+ str(trials[cols[13]][ind]) + "\n},\n"
#     output= output + trial
# file1 = open("script1.txt","w")  
# file1.write("\n")
# file1.writelines(output)
# file1.close()
# file2 = open("stimuli.js","w")  
# file2.write("var exp.stims_cb = [ \n")
# file2.writelines(output)
# file2.write("];")
# file2.close()
# cols.size

