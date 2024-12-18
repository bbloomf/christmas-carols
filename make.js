var child_process = require('child_process'),
    fs = require('fs'),
    noop = () => {},
    psContainsDisp = (psName) => {
        const ps = fs.readFileSync(psName, 'utf8');
        return /GaramondPremrPro-\S*Disp/.test(ps);
    };
try { fs.mkdirSync('lytemp'); } catch (e) { }
function processLy(lyFile,callback) {
    var lyFileToProcess = lyFile;
    var deleteFileAfterProcessed = false;
    var outputName = lyFile.match(/^(?:.*\/)?((\d+).*\.ly)$/);
    if(!outputName) {
        console.info('Skipping "' + lyFile + '" because it is not a .ly file.');
        if(typeof(callback)=='function') {
            callback(null,'','',undefined,false);
        }
        return false;
    }
    var lyName = 'lytemp/'+outputName[1];
    outputName = 'lytemp/'+outputName[2];
    var psName = outputName + '.ps',
        lyContent = fs.readFileSync(lyFile,'utf8');
    if(lyContent.indexOf('%year%') >= 0) {
      var date = new Date();
      var months = ["january","february","march","april","may","june","july","august","september","october","november","december"];
      lyContent = lyContent.replace('%date%',date.getDate());
      lyContent = lyContent.replace('%month%',months[date.getMonth()]);
      lyContent = lyContent.replace('%year%',date.getFullYear());
      lyFileToProcess = lyName + '.pre';
      deleteFileAfterProcessed = true;
    }
    if(fs.existsSync(psName)) {
        const containsDisp = psContainsDisp(psName);
        //Check if the .ly file was the same.
        if(!containsDisp && fs.existsSync(lyName)) {
            var oldLyContent = fs.readFileSync(lyName,'utf8');
            if(lyContent == oldLyContent) {
                //console.info('Skipping "' + lyFile + '" because its .ps file already exists and the lilypond content was the same.');
                if(typeof(callback)=='function') {
                    callback(null,'','',psName,false);
                }
                return false;
            }
        }
        fs.unlinkSync(psName);
    }
    if(fs.existsSync(lyName)) {
        fs.unlinkSync(lyName);
    }
    if(deleteFileAfterProcessed) {
      fs.writeFileSync(lyFileToProcess,lyContent);
    }
    var args = ['-dno-point-and-click','--ps','-o'+outputName,lyFileToProcess];
    console.info('Processing ' + lyFile);
    child_process.execFile('lilypond',args,undefined,function(error,stdout,stderr){
        if(error) {
            console.error(error);
            console.error(stderr);
            console.info(stdout);
        } else {
            fs.writeFile(lyName,lyContent, noop);
            if(deleteFileAfterProcessed) {
              fs.unlink(lyFileToProcess, noop);
            }
        }
        
        if(typeof(callback)=='function'){
            callback(error,stdout,stderr,psName,true);
        }
    });
    return true;
}
var gsCmds = ['gs','gswin64c','gswin32c'],
    gsI = 0;
function ps2pdf(psFiles,width,height,outputName) {
    outputName = outputName || '!full.pdf';
    width *= 72;
    height*= 72;
    if(typeof(psFiles)=='string') psFiles = [psFiles];
    var args = ['-q','-dSAFER','-dDEVICEWIDTHPOINTS='+width,'-dDEVICEHEIGHTPOINTS='+height,'-dCompatibilityLevel=1.4','-dNOPAUSE','-dBATCH',
                '-r1200','-sDEVICE=pdfwrite','-dEmbedAllFonts=true','-dSubsetFonts=true','-sOutputFile='+outputName,'-c.setpdfwrite','-f'].concat(psFiles);
    //console.info(psFiles);
    console.info('Processing PDF of ' + psFiles.length + ' files...');
    //console.info('gs ' + args.join(' '));
    var gsCmd = gsCmds[gsI];
    var cb = function(error,stdout,stderr){
        if(error) {
            console.error(error);
            console.error(stderr);
            console.info(stdout);
            if ((gsCmd=gsCmds[++gsI])) child_process.execFile(gsCmd,args,undefined,cb);
            return;
        }
        console.info('Finished');
    };
    child_process.execFile(gsCmd,args,undefined,cb);
}

//processLy('ly/001-Contents.ly');
//ps2pdf('lytemp/001.ps',8.5,11,'test.pdf');
var dir = 'ly/8.5garamond/',
    files = fs.readdirSync(dir).sort(),
    maxConcurrent = 1,
    currentlyActive = 0,
    i = 0,
    psFiles = [],
    callback = function(error,stdout,stderr,psName,startedWorker) {
        if(typeof(psName)=='string' && psName.length > 0) {
            psFiles.push(psName);
        }
        if(currentlyActive > 0 && startedWorker) --currentlyActive;
        while(i < files.length && currentlyActive < maxConcurrent) {
            //++currentlyActive;
            if(processLy(dir + files[i++], callback)) {
                console.info('Processing file ' + i + ' of ' + files.length + '; ' + (++currentlyActive) + ' active');
            }
        }
        if(i==files.length && currentlyActive < maxConcurrent) {
            if(currentlyActive === 0) {
                psFiles = psFiles.sort(function(a,b){
                  var regex = /^(?:.*\/)?(\d+)/,
                      mA = regex.exec(a),
                      mB = regex.exec(b);
                  if(mA && mB) {
                    a = parseInt(mA[1]);
                    b = parseInt(mB[1]);
                    //console.info('a:' + a + ';b:' + b + ';' + (a+b));
                  }
                  return (a < b)? -1 : ((a > b)? 1 : 0);
                });
                psFiles.sort();
                console.info(psFiles);
                ps2pdf(psFiles,8.5,11,'!full.pdf');
                ++i;
                ++currentlyActive;
            } else if(startedWorker) {
                console.info(currentlyActive + ' active');
            }
        }
    };
callback();