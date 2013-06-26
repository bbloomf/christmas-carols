var child_process = require('child_process'),
    fs = require('fs');

function callGit(args,success){
    child_process.execFile('git',args,undefined,function(error,stdout,stderr){
        if(error) {
            console.error(error);
            console.error(stderr);
            console.info(stdout);
        } else if(typeof(success)=='function') {
            success(error,stdout,stderr);
        }
    });
}
function gitUpdate(){
    callGit(['checkout','gh-pages'],function(){
        fs.renameSync('ccc.pdf','PDFs/ccc.pdf');
        fs.renameSync('ccc-playbook.pdf','PDFs/ccc-playbook.pdf');
        callGit(['commit','-a','-m','Updated PDFs'],function(){
            callGit(['checkout','master']);
        });
    });
}
function callPdfs() {
    child_process.execFile('make-pdfs.bat',[],undefined,function(error,stdout,stderr){
        if(error) {
            console.error(error);
            console.error(stderr);
            console.info(stdout);
        } else {
            gitUpdate();
        }
    });
}
if(fs.existsSync('ccc.pdf') && fs.existsSync('ccc-playbook.pdf')) {
    gitUpdate();
} else {
    callPdfs();
}