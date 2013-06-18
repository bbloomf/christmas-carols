$regexTitle = [regex]'(?<=title = \\markup{\\override #''\(font-name . "Garamond Premier Pro Semibold"\){ \\abs-fontsize #18 \\smallCapsOldStyle")[^"]*(?=")';
$regexFileTitle = [regex]'(?<=\d+-).*(?=\.ly)';
$regexComment = [regex]'\s*%.*$';
$regexMapLine = [regex]'(\d+):(?:([^;]+);?)+';
$regexPageNum = [regex]'(?<=first-page-number\s+=\s+#)\d+';
$regexRaggedLast = [regex]'(?<=ragged-last-bottom\s+=\s+##)t';

$allFiles = New-Object Collections.ArrayList;
foreach($_ in (dir *.ly)) {
    $allFiles.Add($_.Name);
}

Function getBlock([string]$haystack, [long]$index) {
    $open = 0;
    $pos = $haystack.IndexOf('{',$index);
    while($pos -ge 0) {
        if($haystack[$pos] -eq '{') {
            $open++;
        } else {
            $open--;
        }
        if($open -eq 0) {
            return $haystack.Substring($index, $pos + 1 - $index);
        }
        $pos = $haystack.IndexOfAny("{}",$pos+1)
    }
    return '';
}

foreach($_ in $allFiles) {
    $current = Get-Content -Path $_ -Encoding UTF8 -Raw
    echo $_;
    $result = '';
    
    $numScoreBlocks = 0;
    $scoreBlockEndBrace = -1;

    #add LyricText override if not present
    $pos = 0;
    while( ($pos = $current.IndexOf('\layout {',$pos+1)) -ge 0) {
        $layoutBlock = getBlock $current $pos;
        $lyricContext = $layoutBlock.IndexOf('\Lyrics');
        if($lyricContext -ge 0) {
            $beginningOfLine = $layoutBlock.LastIndexOf("`n",$lyricContext);
        } else {
            $beginningOfLine = -1;
        }
        $tempA = $current.Substring(0, $pos + $layoutBlock.Length - 1);
        $tempB = $current.Substring($pos + $layoutBlock.Length);
        $current = $tempA + "  \context {
      \Lyrics
      \override LyricText #'X-offset = #center-on-word
    }
  }" + $tempB;
        Set-Content -Path $_ -Value $current -Encoding UTF8;
    }
    #Set-Content -Path "mapped\$page.ly" -Value $result -Encoding UTF8
    #&'C:\Program Files (x86)\lilypond\usr\bin\lilypond.exe' -dno-point-and-click --ps -o"tmp/$page" "mapped\$page.ly"
}
#iex "&'c:\Program Files\gs\gs9.06\bin\gswin64c.exe' -q -dSAFER -dDEVICEWIDTHPOINTS=612 -dDEVICEHEIGHTPOINTS=792 -dCompatibilityLevel='1.4' -dNOPAUSE -dBATCH -r1200 -sDEVICE=pdfwrite -dEmbedAllFonts=true -dSubsetFonts=true -sOutputFile=""!full.pdf"" -c.setpdfwrite -f$pages";