\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Good Christian Men, Rejoice"}}
  poet = \markup\oldStyleNum"John Mason Neale (1818–1866)"
  composer = \markup\oldStyleNum"14th Century German Melody"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #073
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
global = {
  \key f \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \partial 8 f8 |
  f4 f8 a4 bes8 |
  c4( d8 c4) c8 |
  f,4 f8 a4 bes8 |
  
  c4( d8 c4) bes8\rest | \break
  c4 d8 c4 bes8 |
  a4 g8 f4. |
%  f f |
  g4 g8 a4 g8 |
  
  f4 g8 a4 bes8\rest | \break
  c4 d8 c4 bes8 |
  a4 g8 f4 f8 |
  g4 g8 a4 g8 |
  
  f4 g8 a4 bes8\rest | \break
  d,4 d8 e4 e8 |
  f4.( c') |
  \partial 8*5 a4 a8 g4 g8 f4. bes4\rest \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  c8 |
  d4 c8 f8[ e] d |
  c4( f8 c4) f8 |
  d4 c8 f[ e] d |
  
  c4( f8 e4) s8 |
  f4 f8 e4 e8 |
  f4 e8 d4. |
%  f f |
  f4 f8 f4 e8 |
  
  f4 f8 f4 s8 |
  f4 f8 e4 g8 |
  f4 e8 d4 f8 |
  f4 f8 f4 e8 |
  
  f4 f8 f4 s8 |
  d4 d8 d4 cis8 |
  d4.( e) |
  f4 f8 f4 e8 |
  f4. s4 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  Good Chris -- tian men, re -- joice __
  With heart, and soul and voice; __
  Give ye heed to what we say:
%  News! News!
  Je -- sus Christ is born to -- day:
  Ox and ass be -- fore Him bow,
  And He is in the man -- ger now.
  Christ is born to -- day! __
  Christ is born to -- day!
}
altoWordsII = {
  \dropLyricsV
  \set stanza = \markup{\dynamic"  mf " "2. "}
  \lyricmode {
  %\markup\italic
    Good Chris -- tian men, re -- joice __
    With heart, and soul and voice; __
    Now ye hear of end -- less bliss:
  %  Joy! Joy!
  }
  \set stanza = \markup\dynamic"f  "
  \lyricmode {
    Je -- sus Christ was born for this!
    He hath ope’d the heav’n -- ly door, And man is bless -- ed ev -- er -- more.
  }
  \set stanza = \markup\dynamic" ff "
  \lyricmode {
    Christ was born for this! __
    Christ was born for this!
  }
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  Good Chris -- tian men, re -- joice __
  With heart, and soul and voice; __
  Now ye need not fear the grave:
%  Peace! Peace!
  Je -- sus Christ was born to save!
  Calls you one and calls you all,
  To gain His ev -- er -- last -- ing hall.
  Christ was born to save! __
  Christ was born to save!
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  a8 |
  bes4 a8 c4 bes8 |
  a4( bes8 g4) a8 |
  bes4 a8 c4 bes8 |
  
  a4( bes8 g4) s8 |
  c4 bes8 g4 c8 |
  c4 bes8 a4. |
%  f a |
  d4 d8 c4 bes8 |
  
  a4 bes8 c4 s8 |
  c4 bes8 g4 c8 |
  c4 bes8 a4 c8 |
  d4 d8 c[ d] bes |
  
  a4 bes8 c4 s8 |
  a4 a8 g4 g8 |
  a4( bes8 g4.) |
  f4 a8 d4 c8 |
  a4. s4 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f8 |
  f4 f8 f4 f8 |
  f4.( e4) f8 |
  f4 f8 f4 f8 |
  
  f4.( c4) d8\rest |
  a4 bes8 c4 c8 |
  f4 c8 d4. |
%  f d |
  bes4 bes8 c4 c8 |
  
  f4 f8 f4 d8\rest |
  a4 bes8 c4 e8 |
  f4 c8 d4 a8 |
  bes4 bes8 c4 c8 |
  
  f4 f8 f4 d8\rest |
  f4 f8 e4 e8 |
  d4.( c) |
  f4 d8 bes4 c8 |
  f4. d4\rest \bar "|."
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
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
    \tempo 4 = 105
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
