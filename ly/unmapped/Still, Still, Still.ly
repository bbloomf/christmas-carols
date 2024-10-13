\version "2.14.2"
\include "util.ly"
\header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Still, Still, Still"}}
    poet = \markup\oldStyleNum"Traditional Austrian"
    composer = \markup\oldStyleNum"Salzburg Melody, c. 1819"
    tagline = \markup\concat{ "from " \italic "Salzburgische Volks-Lieder" \oldStyleNum", 1865"}
  }\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 2)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0.5)
       (stretchability . 0))
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
  first-page-number = #170
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

\markup\fill-line{\concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}}
\markup\vspace#4
%END_IF_NOT_LESSER




















global = {
  \key ees \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  bes4( ees) g,( bes) |
  ees,2. ees8[ g] |
  f4 f8[ aes] d,4 d8[ f] |
  ees2. \bar""\break g4 |
  
  f4 f8[ g] aes4 f |
  g g8[ aes] bes4 g |
  f f8[ g] aes4 f |
  g g8[ aes] bes4 g |
  
  bes( ees) g,( bes) |
  ees,2. ees8[ g] |
  f4 f8[ aes] d,4 d8[ f] |
  ees1 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  g'2 ees |
  ees2. c4 |
  c c4 bes4 bes |
  bes2. ees4 |
  
  d4 d8[ ees] f4 f |
  ees ees8[ f] g4 ees |
  d d8[ ees] f4 f |
  ees ees8[ f] g4 ees |
  
  g2 ees |
  ees2. c4 |
  c c4 bes4 bes |
  bes1 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  Still, still, still,
  Weil’s Kind -- lein schlaf -- en will.
  Die Eng -- lein tun schön ju -- bi -- lier -- en,
  Bei dem Krip -- plein mu -- si -- zier -- en.
  Still, still, still,
  Weil’s Kind -- lein schlaf -- en will.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  Schlaf, schlaf, schlaf,
  Mein lieb -- es Kind -- lein schlaf!
  Ma -- ri -- a tut es nie -- der -- sing -- en
  Ihr -- e keu -- sche Brust dar -- bring -- en.
  Schlaf, schlaf, schlaf,
  Mein lieb -- es Kind -- lein schlaf!
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Groß, groß, groß
  Die Lieb’ ist ü -- ber -- groß!
  Gott hat den Him -- mels -- thron ver -- las -- sen
  Und muss reis -- en auf der Straß -- en.
  Groß, groß, groß
  Die Lieb’ ist ü -- ber -- groß.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  Wir, wir, wir,
  Wir ruf -- en all zu dir:
  Tu uns des Him -- mels Reich auf -- schließ -- en,
  Wenn wir ein -- mal ster -- ben müss -- en.
  Wir, wir, wir,
  Wir ruf -- en all zu dir.
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  bes2 bes |
  g2. g4 |
  aes4 aes4 aes4 aes |
  g2. bes4 |
  
  bes bes bes bes |
  bes bes bes bes |
  bes bes bes bes |
  bes bes bes bes |
  
  bes2 bes |
  g2. g4 |
  aes4 aes4 aes4 aes |
  g1 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  ees2 d |
  c2. bes4 |
  aes aes4 bes4 bes |
  ees2. ees4 |
  
  bes4 bes d d |
  ees ees ees ees |
  bes bes d d |
  ees ees ees ees |
  
  ees2 d |
  c2. bes4 |
  aes aes4 bes4 bes |
  ees1 \bar "|."
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
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWords
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
    \tempo 4 = 77
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
