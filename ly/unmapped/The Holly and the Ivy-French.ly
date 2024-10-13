\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"The Holly and the Ivy"}}
  poet = \markup\oldStyleNum"17th Century English"
  composer = \markup\oldStyleNum"Old French Carol"
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
  last-bottom-spacing =
    #'((basic-distance . -10)
       (minimum-distance . -10)
       (padding . 0)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #097
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
#(set-global-staff-size 14.2) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.2 20))) }
global = {
  \key f \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \partial 8 d8 |
  a'4 a8 a[ g] a |
  bes4. a4 d,8 |
  e[ f] g f[ e] f |
  
  d4.~ d4 \bar""\break d8 |
  a'4 a8 a4 d8 |
  \slurDotted
  c8( c) bes8 a4 gis8 |
  a4 b8 c4 b8 |
  
  a4.~ a4 bes8\rest \bar "||" \break
  e,4 f8 g4 a8 |
  g4 f8 e4 a8 |
  a[ f] a a[ g] f |
  
  e4.~ e4 a8 |
  b4 b8 cis4 cis8 |
  d8 d8 a a4 d8 |
  
  d[ c] bes a[ g] fis |
  g4.~ g4 bes8 |
  a[ bes] g f4 e8 |
  \partial 8*5 d4.~ d4 \bar ":|" \break
  
  
  
  
  \partial 8 d8 |
  a'4 a8 a[ g] a |
  bes4.( a4) d,8 |
  e( f) g f[ e] f |
  
  d4.~ d4 \bar""\break d8 |
  a'4 a8 a4 d8 |
  c4 bes8 a4 gis8 |
  \tieDotted
  a4 b8 c8~ c b8 |
  
  \tieSolid
  a4.~ a4 bes8\rest \bar "||" 
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d8 |
  d4 cis8 d4 d8 |
  d4. d4 d8 |
  d4 d8 cis4 cis8 |
  
  d4.~ d4 d8 |
  d4 g8 f4 a8 |
  \slurDotted
  g8( g) g8 f4 e8 |
  a4 a8 a4 gis8 |
  
  a4.~ a4 s8 |
  c,4 c8 c4 c8 |
  d4 d8 c4 e8 |
  f[ d] f f[ e] d |
  
  cis4.~ cis4 e8 |
  e4 e8 e4 e8 |
  a a g fis[ g] a |
  
  d,4 d8 ees4 ees8 |
  d4.~ d4 d8 |
  f4 e8 d4 cis8 |
  d4.~ d4 |
  
  
  
  d8 |
  d4 cis8 d4 d8 |
  d4.( d4) d8 |
  d8( d) d8 cis4 cis8 |
  
  d4.~ d4 d8 |
  d4 g8 f4 a8 |
  g4 g8 f4 e8 |
  \tieDotted
  a4 a8 a~ a gis8 |
  
  \tieSolid
  a4.~ a4 s8 |
}
altoWords = {
  \dropLyricsXI
  \lyricmode {
    \set stanza = #"1, 6."
    The Hol -- ly and the I -- vy,
    Now both are full -- well grown, __
    \set ignoreMelismata = ##t
    Of all the trees that are in the wood,
    \unset ignoreMelismata
    The Hol -- ly bears the crown: __
  }
  \set stanza = \markup\dynamic"p  "
  \lyricmode {
    O the ris -- ing of the sun,
    The run -- ning of the deer,
  }
  \set stanza = \markup\dynamic"   f "
  \lyricmode {
    The play -- ing of the mer -- ry or -- gan,
    Sweet sing -- ing in the quire,
    Sweet sing -- ing in the quire.
    
  }
  \set stanza = \markup{\dynamic" mf" "4."}
  \lyricmode {
    The Hol -- ly bears a \set ignoreMelismata = ##t
    prick -- le,
    \unset ignoreMelismata
    As sharp as an -- y thorn, __
  }
  \set stanza = \markup\dynamic"f "
  \lyricmode {
    And Ma -- ry bore sweet Je -- sus Christ,
    On Christ -- mas \set ignoreMelismata = ##t day in the morn. __ _
  }
}
altoWordsII = {
  \dropLyricsXI
  \set stanza = \markup{\dynamic"mf " "2."}
  \lyricmode {
    The Hol -- ly bears a blos -- som,
    As white as li -- ly -- flow’r; __
  }
  \set stanza = \markup\dynamic"f  "
  \lyricmode {
    And Ma -- ry bore sweet Je -- sus Christ,
    To be our sweet Sav -- ior. __ ""
    
    \repeat unfold 33 \skip1
    \set stanza = #"5."
    The Hol -- ly bears a bark, __
    \set ignoreMelismata = ##t
    As bit -- ter 
    \unset ignoreMelismata
    as an -- y gall; __
    And Ma -- ry bore sweet Je -- sus Christ,
    For to re -- deem us all. __
  }
}
altoWordsIII = \lyricmode {
  \dropLyricsXI
  \set stanza = #"3."
  The Hol -- ly bears a ber -- ry,
  As red as an -- y blood, __
  And Ma -- ry bore sweet Je -- sus Christ,
  To do poor sin -- ners good. __
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
  \set stanza = #"6."
}
tenorMusic = \relative c {
  f8 |
  f4 g8 f[ g] f |
  g4. a4 a8 |
  bes4 bes8 a4 g8 |
  
  f4.~f4 bes8 |
  a4 cis8 d4 d8 |
  \slurDotted
  d8( d) d8 d4 b8 |
  e4 f8 e4 d8 |
  
  c4.~ c4 s8 |
  c4 a8 g4 f8 |
  bes4 a8 g4 g8 |
  f4 f8 bes4 d8 |
  
  a4.~ a4 cis8 |
  b4 b8 a4 a8 |
  a b cis d4 c8 |
  
  bes[ a] g fis[ g] a |
  g4.~ g4 g8 |
  a4 bes8 a4 g8 |
  f4.~ f4 |
  
  
  
  
  
  f8 |
  f4 g8 f[ g] f |
  g4.( a4) a8 |
  bes8( bes) bes8 a4 g8 |
  
  f4.~f4 bes8 |
  a4 cis8 d4 d8 |
  d4 d8 d4 b8 |
  \tieDotted
  e4 f8 e~ e d8 |
  
  \tieSolid
  c4.~ c4 s8 |
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d8 |
  d4 e8 f[ e] d |
  g4. f4 f8 |
  g4 g,8 a4 a8 |
  
  d4.~ d4 g8 |
  f4 e8 d4 f8 |
  \slurDotted
  g8( g) g8 d4 d8 |
  c4 d8 e4 e8 |
  
  a4.~ a4 d,8\rest |
  a'4 f8 e4 f8 |
  bes,4 bes8 c4 cis8 |
  d4 d8 g4 g8 |
  
  a4.~ a4 a8 |
  gis4 gis8 g4 g8 |
  f f e d[ e] fis |
  
  g4 g8 c,4 c8 bes4.~ bes4 g'8 |
  f4 g8 a4 a,8 |
  d4.~ d4 |
  
  
  
  
  d8 |
  d4 e8 f[ e] d |
  g4.( f4) f8 |
  g8( g) g,8 a4 a8 |
  
  d4.~ d4 g8 |
  f4 e8 d4 f8 |
  g4 g8 d4 d8 |
  \tieDotted
  c4 d8 e~ e e8 |
  
  \tieSolid
  a4.~ a4 d,8\rest |
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
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.9))} \lyricsto "sopranos" \altoWords
    \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
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
    \tempo 4 = 90
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
