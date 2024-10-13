\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Christmas Song"}}
  poet = \markup\oldStyleNum"William Bright (1824–1901)"
  composer = \markup\oldStyleNum"John Bacchus Dykes (1823–1876)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  %print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  markup-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -0.25)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #164
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 14.8) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.8 20))) }
%6.14 #(set-global-staff-size 14.3) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.3 20))) }
global = {
  \key c \major
  \time 6/8
  \autoBeamOff
  \mergeDifferentlyHeadedOn
  \mergeDifferentlyDottedOn
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(0.5 . 0.5)
}

sopMusic = \relative c' {
    \repeat volta 3 {
    g'4 fis8 g4 e'8 |
    e4 d8 c[ b a] |
    g[ a] b c4 b8 |
    b4( a8) g4. \bar "||"
    
    g4 fis8 g4 e'8 |
    e4 d8 c b a |
    g[ b] c d([ e d] |
    c[ e, fis]) g b a |
    
    a4( g8~ g[ e]) fis |
    a4. g4( f!8) \bar "||"
    e4^\p d8 e4 f8 |
    g4-> g8 f4. |
    
    fis4^\pp e8 fis4 g8 |
    a4.-> g |
    g4^\markup\italic"cresc." g8 c4 c8 |
    d[ c] d ees4. |
    
    ees4^\f c8 ees8[ d] c |
    c4. b | \break
    c4 c8 f[ e] d |
    c4.( e4 d8) |
    
    c4.~ c |
    g^\markup\italic"dim." g |
    g-> g |
    g->~ g~ |
    g g
  } \break
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  e4 dis8 e4 e8 |
  f[ g] f e4( dis8) |
  e4 e8 e[ees] d |
  c[ d c] b[ f'! d] \bar "||"
  
  e4 dis8 e4 e8 |
  f[ g] f e e e |
  d4 d8 d4.~ |
  d4( c8) b d c16[ e] |
  
  d4.( c4) c8 |
  c4. b8[ c d] \bar "||"
  c4 c8 c4 c8 |
  c[ d] e f4( c8) |
  
  d4 d8 d4 d8 |
  d[ e fis] g4. |
  g4 g8 c4 c,8 |
  d[ c] d ees4. |
  
  ees4 ees8 ees[ f] fis |
  g4( a8) g4( f8) |
  e[ d] e f[ g] a |
  g4( e8 f4.) |
  
  e4( f8 e4 f8) |
  e4( f8) e[ c d] |
  e4. c8[ d e] |
  f4.( e4 d8 |
  c4 d8) c4.
}
dropLyrics = {
  \dropLyricsIX
}
altoWords = \lyricmode {
  \dropLyrics
  \set stanza = #"1."
  Once a -- gain O bless -- ed time,
  thank -- ful hearts em -- brace thee:
  If we lost thy fes -- tal chime,
  What could e’er re -- place __ thee?
  What
  \set associatedVoice = "sopranos"
  could e’er __ re -- place thee? __
  \unset associatedVoice
  
  Change will dark -- en ma -- ny~a day,
  \set associatedVoice = "basses"
  Ma -- ny~a bond dis -- sev -- er;
  \unset associatedVoice
  Ma -- ny~a joy shall pass a -- way,
  But the “Great Joy” nev -- er!
  But the “Great Joy” nev -- er, __
  But the “Great
  \set associatedVoice = "tenors"
  Joy” nev -- er!
  
  
  
  
  
  
  \unset associatedVoice
  \set stanza = #"4. "
  Yea, if oth -- ers stand a -- part,
  We will press the near -- er;
  Yea, O best fra -- ter -- nal Heart,
  We will hold Thee dear -- er,
  We
  \set associatedVoice = "sopranos"
  will hold __ Thee dear -- er; __
  \unset associatedVoice
  
  Faith -- ful lips shall an -- swer thus
  \set associatedVoice = "basses"
  To all faith -- less scorn -- ing,
  \unset associatedVoice
  “Je -- sus Christ is God with us,
  Born on Christ -- mas morn -- ing.
  Born on Christ -- mas morn -- ing, __
  Born on Christ -- 
  \set associatedVoice = "tenors"
  mas morn -- ing.”
}
altoWordsII = {
  \dropLyrics
  \set stanza = \markup{\dynamic"  mf " "2."}
  \lyricmode {
  %\markup\italic
    Once a -- gain the Ho -- ly Night
    Breathes its bless -- ing ten -- der;
    Once a -- gain the Man -- ger Light
    Sheds its gen -- tle splen -- dor,
    Sheds
    \set associatedVoice = "sopranos"
    its gen -- tle splen -- dor; __
    \unset associatedVoice
    
    O could tongues by An -- gels taught
    \set associatedVoice = "basses"
    Speak our ex -- ul -- ta -- tion
    \unset associatedVoice
    In the Vir -- gin’s Child that brought
    All man -- kind Sal -- va -- tion.
  }
  \set stanza = \markup\dynamic"ff"
  \lyricmode{
    All man -- kind Sal -- va -- tion, __
    All man -- kind 
    \set associatedVoice = "tenors"
    Sal -- va -- tion.
    
    
    
    
    
    
    \unset associatedVoice
    \set stanza = #"5. "
    So we yield Thee all we can,
    Wor -- ship, thanks, and bless -- ing;
    Thee true God, and Thee true Man
    On our knees con -- fess -- ing,
    On
    \set associatedVoice = "sopranos"
    our knees __ con -- fess -- ing; __
    \unset associatedVoice
    
    While Thy Birth -- day morn we greet
    \set associatedVoice = "basses"
    With our best de -- vo -- tion,
    \unset associatedVoice
    Bathe us, O most true and sweet!
    In Thy Mer -- cy’s o -- cean.
  }
  \set stanza = \markup\dynamic"ff"
  \lyricmode{
    In Thy Mer -- cy’s o -- cean, __
    In Thy Mer -- 
    \set associatedVoice = "tenors"
    cy’s o -- cean. 
  }
}
altoWordsIII = \lyricmode {
  \dropLyrics
  \set stanza = #"3."
  Wel -- come Thou to souls a -- thirst,
  Fount of end -- less plea -- sure;
  Gates of Hell may do their worst,
  While we clasp our Trea -- sure,
  While
  \set associatedVoice = "sopranos"
  we clasp __ our Trea -- sure: __
  \unset associatedVoice
  
  Wel -- come, though an age like this
  \set associatedVoice = "basses"
  Puts Thy Name on tri -- al,
  \unset associatedVoice
  And the Truth that makes our bliss
  Pleads a -- gainst de -- ni -- al!
  Pleads a -- gainst de -- ni -- al, __
  Pleads a -- gainst 
  \set associatedVoice = "tenors"
  de -- ni -- al!
  
  
  \unset associatedVoice
  \set stanza = #"6. "
  Thou that once, ’mid sta -- ble cold,
  Wast in babe -- clothes ly -- ing,
  Thou whose Al -- tar -- veils en -- fold
  Pow’r and Life un -- dy -- ing,
  Pow’r
  \set associatedVoice = "sopranos"
  and Life __ un -- dy -- ing, __
  \unset associatedVoice
  
  Thou whose Love be -- stows a worth
  \set associatedVoice = "basses"
  On each poor en -- deav -- or,
  \unset associatedVoice
  Have Thou joy of this Thy Birth
  In our praise for ev -- er.
  In our praise for ev -- er, __
  In our praise 
  \set associatedVoice = "tenors"
  for ev -- er.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  c8[ g] a g[ c] g |
  a4 b8 c[ g fis] |
  g4 g8 g4 g8 |
  g( fis4) g8[ a b] \bar "||"
  
  c[ g] a g[ c] g |
  a4 b8 c d c |
  b[ g] a b4.~ |
  b4( a8) g gis a |
  
  b4.( a4) a8 |
  fis[ e d] g4. \bar "||"
  g4^\p f8 g4 a8 |
  bes4 bes8 a4. |
  
  \once\override DynamicText #'X-offset = #-4
  a4^\pp g8 a4 b8 |
  c4. b |
  g4 g8 c4 c8 |
  d[ c] d ees4. |
  
  c4^\f c8 c[ d] ees |
  e!4( ees8) d4. |
  c4 c8 a[ g] f' |
  e([ d c] b[ a b]) |
  
  c4( a8 g4 a8) |
  g4( a8) g4. |
  g8[ c d] e[ d c] |
  b4( a8 g4 f8 |
  e4 f8) e4.
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  c4 c8 c4 c8 |
  g4 g8 c4. |
  c4 b8 a4 b16[ c] |
  d4. g \bar "||"
  
  c,4 c8 c4 c8 |
  g4 g8 c c c |
  d4 d8 d4.~ |
  d  e8 c c |
  
  d4.~ d4 d8 |
  g,4. g8[ a b] \bar "||"
  c4 c8 c4 c8 |
  e[ d] c f4( ees8) |
  
  d4 d8 d4 d8 |
  fis[ e d] g4. |
  g4 g8 c4 c8 |
  d[ c] d ees4. |
  
  aes,4 aes8 aes4 aes8 |
  g4. g |
  a4 a8 d,[ e] f |
  g4.( g,) |
  
  c4.~ c |
  c c8[ e f] |
  g4. g |
  g,~ g( |
  c) c
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \tempo 8 = 144 \repeat unfold 2 \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \repeat unfold 2 \altoMusic >> }
    >>
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold 2 \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold 2 \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.6))} \lyricsto "altos" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
%6.14 \context {\Lyrics\override LyricText #'font-size = #0.7 }
    \context {
      \Score
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      %\override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText #'X-offset = #center-on-word
    }
  }
  \midi {
    \tempo 8 = 144
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
