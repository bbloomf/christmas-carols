\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Shepherds! Shake Off Your Drowsy Sleep"}}
  poet = \markup\oldStyleNum"Traditional"
  composer = \markup\oldStyleNum"Besançon Carol"
  arranger = \markup\oldStyleNum"Arranged by Sir John Stainer (1840–1901)"
  tagline = \markup \concat{ "from " \italic"Carols Old and Carols New" \oldStyleNum", 1916, via " \italic"HymnsAndCarolsOfChristmas.com" }
}
\paper {
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -0.35)
  %     (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #021
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key f \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \tempo \markup\italic "Vivace"
  \partial 4. f8\noBeam f\noBeam f |
  c'4 c8 d4 e8 |
  f4. f4 f8 | \break
  
  e4 e8 d[ c] d |
  c4. c8\noBeam d\noBeam e |
  f4 c8 c4 bes8 | \break
  
  a4 g8 a[ bes] a |
  g[ f] g a[ bes] a |
  g4-> f8 \bar "||" \break 
  c'8\noBeam d\noBeam e |
  
  f4 c8 c4 bes8 |
  a4. c4^\markup\italic"poco rit." g8 |
  bes4 a8 g[ f] g |
  \partial 4. f4. \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f8\noBeam f\noBeam f\noBeam |
  f4 f8 f4 g8 |
  a4. a4 a8 |
  
  g4 g8 f4 f8 |
  e4. e8\noBeam f\noBeam g |
  f4 a8 a4 g8 |
  
  f4 e8 f4 f8 |
  d4 d8 e4 e8 |
  e4 f8 e\noBeam f\noBeam g |
  
  f4 f8 f4 f8 |
  f4. e4 e8 |
  f4 f8 e[ d] e |
  f4. \bar "|."
}
altoWords = {
  \dropLyricsIX
  \lyricmode {
    \set stanza = #"1. "
    Shep -- herds! shake off your drow -- sy sleep,
    Rise and leave your sil -- ly sheep;
    An -- gels from heav’n a -- round loud sing -- ing,
    Tid -- ings of __ great joy __ are bring -- ing.
  }
  \set stanza = \markup\dynamic"ff "
  \lyricmode {
    \dropLyricsXI
    Shep -- herds! the cho -- rus come and swell! Sing No -- ël, O sing No -- ël!
  }
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  Hark! e -- ven now the bells ring round,
  Lis -- ten to their mer -- ry sound;
  Hark! how the birds new songs are mak -- ing,
  As __ if win -- ter’s chains __ were break -- ing.
}
altoWordsIII = {
  \dropLyricsIX
  \set stanza = \markup{\dynamic" mf " "3. "}
  \lyricmode {
    See how the flow’rs all burst a -- new,
    Think -- ing snow is sum -- mer dew;
    See how the stars a -- fresh are glow -- ing,
    All __ their bright -- est beams __ be -- stow -- ing.
  }
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  Com -- eth at length the age of peace,
  Strife and sor -- row now __ shall cease;
  Pro -- phets fore -- told the won -- drous sto -- ry
  Of __ this Heav’n born Prince __ of Glo -- ry.
}
altoWordsV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"5. "
  Shep -- herds! then up and quick a -- way,
  Seek the Babe ere break of day;
  He is the hope of ev -- ’ry na -- tion,
  All __ in Him __ shall find __ sal -- va -- tion.
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c {
  f8\noBeam f\noBeam f |
  c'4 c8 bes4 bes8 |
  c4. c4 c8 |
  
  c4 c8 b[ a] b |
  c4. c8\noBeam c\noBeam c |
  c4 c8 d4 d8 |
  
  c4 c8 c4 c8 |
  d4 d8 c[ d] c |
  bes4-> a8 c\noBeam c\noBeam c |
  
  c4 f8 d4 d8 |
  c4. c4 c8 |
  bes4 c8 bes[ a] bes |
  a4. \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f8\noBeam f\noBeam f |
  a4 a8 bes4 g8 |
  f4. f4 a8 |
  
  c4 c8 g4 g8 |
  c,4. c'8\noBeam c\noBeam bes |
  a4 a8 bes4 bes8 |
  
  c4 c8 f,4 f8 |
  bes,4 bes8 c4 c8 |
  f4 f8 c'\noBeam c\noBeam bes |
  
  a4 a8 bes4 bes8 |
  f4. c4 c8 |
  d4 a8 c4 c8 |
  f4. \bar "|."
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
}

