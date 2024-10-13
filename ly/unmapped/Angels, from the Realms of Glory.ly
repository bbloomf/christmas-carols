\version "2.14.2"
\include "util.ly"
\header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Angels, from the Realms of Glory"}}
    poet = \markup\oldStyleNum"James Montgomery (1771–1854)"
    composer = \markup\oldStyleNum"Henry Smart (1813–1879)"
    tagline = \markup { "from" \concat{\italic "Christmas Carols and Hymns for School and Choir" \oldStyleNum", 1910"}}
  }\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
%{IF_LESSER
  markup-system-spacing #'stretchability = 50
  top-markup-spacing #'stretchability = 30
  last-bottom-spacing #'stretchability = 60
%}%END_IF_LESSER
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #076
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

#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
%IF_NOT_LESSER

%IF_NOT_LESSER
\markup\vspace#8
%END_IF_NOT_LESSER





















%Angels From the Realms of Glory
global = {
  \key c \major
  \time 4/4
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
  \autoBeamOff
}

sopMusic = \relative c'' {
  \tempo 4 = 104
  g4 e c' g |
  e'4. d8 c4 g |
  a a g c |
  g f e2 | \break
  
  g4 e c' g |
  e'4. d8 c4 b |
  c b a b8[ c] |
  b4 a g2 | \break
  
  d'4. d8 b4 g |  
  e'4. d8 c4 a |
  f' e d c |
  c b c2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  e4 c g' e |
  g4. g8 g4 g |
  c, c c c |
  
  d b c2 |
  e4 c g' g8[ f] |
  e4. f8 e4 e |
  
  e e e e |
  g fis g2 |
  g4. g8 g4 g |
  
  g4. e8 f4 f |
  a g f e8[ f] |
  g4. f8 e2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  An -- gels, from the realms of glo -- ry,
  Wing your flight o’er all the earth,
  \set associatedVoice = "basses"
  Ye, who sang cre -- a -- tion’s sto -- ry,
  \unset associatedVoice
  Now pro -- claim Mes -- si -- ah’s birth;
  Come and wor -- ship, come and wor -- ship,
  \set associatedVoice = "basses"
  Wor -- ship
  \set associatedVoice = "altos"
  Christ, the
  \unset associatedVoice
  new -- born King.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  Shep -- herds, in the field a -- bid -- ing, Watch -- ing o’er your flocks by night,
  \set associatedVoice = "basses"
  God with man is now re -- sid -- ing;
  \unset associatedVoice
  Yon -- der shines the in -- fant light;
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Sa -- ges, leave your con -- tem -- pla -- tions, Bright -- er vis -- ions beam a -- far;
  \set associatedVoice = "basses"
  Seek the great De -- sire of na -- tions,
  \unset associatedVoice
  Ye have seen His na -- tal star;
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  Saints be -- fore the al -- tar bend -- ing,
    Watch -- ing long in hope and fear,
  \set associatedVoice = "basses"
  Sud -- den -- ly the Lord, de -- scend -- ing, 
  \unset associatedVoice
    In His tem -- ple shall ap -- pear;
}
tenorMusic = \relative c' {
  c4 g g c |
  c4. b8 c4 c |
  a c g a |
  
  g g g2 |
  c4 c g g |
  c4. b8 a4 gis |
  
  a gis e' d8[ c] |
  d4 c b2 |
  b4. b8 d4 b |
  
  c4. bes8 a4 c |
  d g, a8[ b] c4 |
  d4 d c2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  c4 c e c |
  g'4. f8 e4 e |
  f f e a, |
  
  b g c2 |
  c'4 g e e8[ d] |
  c4. d8 e4 e |
  
  a e c a |
  d d g,2 |
  g'4. g8 g4 g |
  
  c,4. c8 f4 f |
  d e f8[ g] a4 |
  g g, c2 \bar "|."
}
bassWords = \lyricmode {

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
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \context Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \context Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \context Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "sopranos" \altoWords
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
    \tempo 4 = 104
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
