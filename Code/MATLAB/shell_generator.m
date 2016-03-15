
iVec = 2:10;
shFolderName = '../Shell/';
shFileName = 'run_brute_';

codeFolderName = '../R/';
codeFileName = 'main_brute';

for i = iVec
    shFullName = getfullname([shFolderName, shFileName], i, '', 'sh');
    codeFullName = getfullname([codeFolderName, codeFileName], NaN, '', 'R');
    if exist(shFullName, 'file') == 0
        fn = sprintf(shFullName);
        fid = fopen(fn, 'w');
        fprintf(fid, '#\n#$ -cwd\n');
        fprintf(fid, '#$ -j y\n');
        fprintf(fid, '#$ -pe orte 12\n');
        fprintf(fid, '#$ -S /bin/bash\n#\n');
        fprintf(fid, '# Name the job #$ -N RScript #\n');
        fprintf(fid, 'export PATH=/usr/local/Revo_rh6/bin:$PATH\n');
        fprintf(fid, ['Rscript ', codeFullName, ' ', num2str(i) '\n']);
        fprintf(fid, 'echo ""\n');
        fprintf(fid, 'echo "Done at " `date`');
        fclose(fid);
    end
end