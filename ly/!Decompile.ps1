$regexGoudyOlSt = [regex]'(GoudyOlSt BT)';
$regexLayoutBlock = [regex]'\\layout\s*{';
$regexPageHeight = [regex]'(?<=paper-height\s*=\s*)\d*(?:\.\d*)?\\in';
$regexMargin = [regex]'(?<=(top|bottom)-margin\s*=\s*)\d*(?:\.\d*)?\\in';
$regexTopMargin = [regex]'(?<=top-margin\s*=\s*)\d*(?:\.\d*)?\\in';
$regexInnerMargin = [regex]'(?<=inner-margin\s*=\s*)\d*(?:\.\d*)?\\in';
$regexOuterMargin = [regex]'(?<=outer-margin\s*=\s*)\d*(?:\.\d*)?\\in';
$regexStaffSize = [regex]'(?<!^\s*%.*#\(set-global-staff-size\s+)(?<=#\(set-global-staff-size\s+)\d*(?:\.\d*)?';
$regexSecondStaff = [regex]'(?<!^\s*%.*"Garamond Premier Pro" \(/ )(?<="Garamond Premier Pro" \(/ )\d*(?:\.\d*)?';
$regexAbsFont85 = [regex]'(?<=\\abs-fontsize\s+#)8\.5';
$regexAbsFont15 = [regex]'(?<=\\abs-fontsize\s+#)15';
$regexAbsFont105 = [regex]'(?<=\\abs-fontsize\s+#)10\.5';
$regexAbsFont9 = [regex]'(?<=\\abs-fontsize\s+#)9';
$regexTitle = [regex]'(?<=title = \\markup{\\override #''\(font-name . "Garamond Premier Pro Semibold"\){ \\abs-fontsize #\d+(?:\.\d*)? \\smallCapsOldStyle")[^"]*(?=")';
$regexPrintAllHeaders = [regex]'(?<=print-all-headers\s+=\s+##)t';
$regexraggedLstBottom = [regex]'(?<=ragged-last-bottom\s+=\s+##)f';
$regexTempo = [regex]'\\tempo\s+\d+\s+=\s+\d+';
$regex69 = [regex]'(\s|^)%6x9\s*';
$defaultMidiBlock = '  \midi {
    \tempo 4 = 90
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}';
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
Function MergeHeaders([string]$header1, [string]$header2) {
    if($header1.Length -eq 0) {
        return $header2;
    }
    return $header1.Substring(0, $header1.length - 1) + $header2.Substring($header2.indexOf('{') + 1);
}

del unmapped\* -Recurse;
$files = (ls -filter ???-*.ly);
$i = 0;
foreach ($_ in $files) {
  echo $_.Name
  $content = Get-Content $_ -Encoding UTF8 -Raw;
  $content = $content -replace $regexPrintAllHeaders,'f';
  $content = $content -replace '"../util.ly"','"util.ly"';
  $containsRepeats = $content.indexOf('\repeat volta') -ge 0;
  #Read first header...
  $headerIndex = $content.IndexOf('\header {');
  if($headerIndex -ge $content.IndexOf('\score')) {
    $header ='';
    $headerIndex = $content.IndexOf('\paper');
  } else {
    $header = getBlock $content $headerIndex;
  }
  #$paperIndex = $content.IndexOf('\header');
  #$paper = getBlock $content $paperIndex;
  $globalIndex = $content.IndexOf('global =');
  $pos = $globalIndex;
  $currentBeginIndex = $globalIndex;
  #todo: if it contains Repeats, add a new score block without the layout block in it, and with an \unfoldRepeats and the midi block.
  #    : also remove any MIDI block from the first score block.
  while( ($pos = $content.IndexOf('\score',$currentBeginIndex)) -ge 0) {
    $scoreBlock = GetBlock $content $pos;
    $scoreEndIndex = $pos + $scoreBlock.Length;
    $scoreHeaderIndex = $content.IndexOf('\header',$pos);
    if($scoreHeaderIndex -ge 0) {
        $scoreHeader = GetBlock $content $scoreHeaderIndex;
        $m = $regexTitle.Match($scoreHeader);
        $currentTitle = $m.Value;
        echo "Found {$currentTitle}";
        $currentHeader = MergeHeaders $header $scoreHeader;
        $content = $content -replace $regexraggedLstBottom,'t';
    } else {
        $m = $regexTitle.Match($header);
        $currentTitle = $m.Value;
        $currentHeader = $false;
        echo "Outputting {$currentTitle}";
    }
    $currentContent = $content.Substring(0,$globalIndex) + $content.Substring($currentBeginIndex, $scoreEndIndex - $currentBeginIndex);
    if($currentHeader) {
        $currentContent = $currentContent.Replace($scoreHeader,'');
        $currentContent = $currentContent.Substring(0,$headerIndex) + $currentHeader + $currentContent.Substring($headerIndex + $header.Length);
    }
    if($scoreBlock.IndexOf('\midi') -lt 0) {
        $tempo = $regexTempo.Match($currentContent);
        if($tempo.Success) {
            $midiBlock = $defaultMidiBlock.Replace('\tempo 4 = 90',$tempo);
        } else {
            $midiBlock = $defaultMidiBlock;
        }
        $currentContent = $currentContent.Substring(0, $currentContent.Length - 1) + $midiBlock;
    }

    $currentFile = "unmapped\$currentTitle.ly".Replace('?','');
    $num = 0;
    while(Test-Path -Path $currentFile -PathType Leaf) {
        $num++;
        $currentFile = "unmapped\$currentTitle-$num.ly".Replace('?','');
    }
    $currentContent | out-file ($currentFile) -Encoding UTF8;
    $currentBeginIndex = $scoreEndIndex;
  }
}