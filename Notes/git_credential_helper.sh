git_credential_helper.sh
#!/bin/bash
echo username=GraceMcFa
echo password=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
#assignment 1, still need to finish step 11#
#finished step 11 I think#
#Don't forget to copy some course materials from “Data_Course” into your “Data_Course_LASTNAME” repository. You can click and drag them over there if you want. Whatever. But just take the following directories and copy them into your repository (then add/commit/push the new files to your online repo):#
csv_files <- list.files(path = "Data/", pattern = "\\.csv$", full.names = TRUE)
path = "Data/": Specifies the directory to search for files.
pattern = "\\.csv$": Ensures only files with a .csv extension are included. The \\ is used to escape the dot in .csv.
full.names = TRUE: Returns the full path of the files. If you want just the file names without the directory path, set this to FALSE.

If you are in GitHub you can press shift and period to get to an editor

