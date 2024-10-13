\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"In the Bleak Midwinter"}}
  poet = \markup\oldStyleNum"Christina Rosetti (1830–1894)"
  composer = \markup\oldStyleNum"Gustav Holst (1874–1934)"
  tagline = \markup { "from" \italic "CantateDomino.org"}
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
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #182
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
  \time 4/4
}

sopMusic = \relative c'' {
  \partial 4 a4 |
  \slurDotted
  a4.( bes8) c4 a |
  g4( g) f bes\rest |
  g4.( a8) g4 d |
  g2. \bar""\break a4 |
  
  a4.( bes8) c4 a |
  g4.( g8) f4 f |
  g4( a) g4. f8 |
  f2. \bar""\break f4 |
  
  bes4. a8 bes4( c) |
  d( d) a a |
  c( a) g( f) |
  e2. \bar""\break a4 |
  
  a4.( bes8) c4 a |
  g2 f4 bes\rest |
  \slurSolid g4( a) g4.( f8) |
  f1 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \partial 4 f4 |
  \slurDotted f4.( f8) f4 c |
  d4( d) d s |
  d4.( d8) d4 c |
  bes2. c4 |
  
  c4.( f8) f4 c |
  d4.( d8) d4 d |
  f4( f) e4. f8 |
  f2. f4 |
  
  f4. f8 f4( ees4) |
  d( d) f f |
  f( e) d( d) |
  c2. c4 |
  
  c4.( f8) f4 c |
  d2 d4 s |
  \slurSolid f2 e4.( f8) |
  f1 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsXI
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  "" In the bleak mid -- win -- _ ter,
  fros -- ty wind made moan, ""
  Earth stood hard as i -- _ ron, ""
  wa -- ter like a stone, ""
  Snow had fal -- len, snow on snow, ""
  snow __ _ on __ _ snow, ""
  In the bleak mid -- win -- 
  \set associatedVoice = "tenors"
  ter,
  Long a -- _ go.
}
altoWordsII = \lyricmode {
  \dropLyricsXI
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  Our God, Heav’n can -- not hold
%8.5x11 __
  _ Him
  Nor __ _ earth sus -- tain; ""
  Heav’n and earth shall flee a -- way ""
  When He comes to reign; ""
  In the bleak mid -- win -- _ ter a sta -- ble place suf -- ficed
  The Lord __ _ God Al -- migh -- 
  \set associatedVoice = "tenors"
  ty,
  Je -- sus __ _ Christ.
}
altoWordsIII = \lyricmode {
  \dropLyricsXI
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  E -- nough for Him, whom Cher -- u -- bim
  Wor -- ship night and day,
  A breast -- _ ful of milk, __ _
  And a man -- ger -- ful of hay:
  E -- nough for Him, whom an -- _ gels ""
  Fall __ _ down be -- fore,
  The ox and ass and ca -- 
  \set associatedVoice = "tenors"
  mel which a -- _ dore.
}
altoWordsIV = \lyricmode {
  \dropLyricsXI
\set ignoreMelismata = ##t
  \set stanza = #"4. "
  "" An -- gels and arch -- an -- _ gels May have ga -- thered there ""
  Cher -- u -- bim and Ser -- a -- phim ""
  Throng -- _ ed the air
  But on -- ly His __ _ mo -- _ ther ""
  In her maid -- en bliss ""
  Wor -- shipped the Be -- lov -- 
  \set associatedVoice = "tenors"
  ed with a __ _ kiss.
}
altoWordsV = \lyricmode {
  \dropLyricsXI
\set ignoreMelismata = ##t
  \set stanza = #"5. "
  "" What __ _ can I give __ _ Him,
  Poor __ _ as I am? ""
  If I were a shep -- _ herd ""
  I would bring a lamb; ""
  If I were a wise __ _ man ""
  I would do my part;
  Yet what __ _ can I give 
  \set associatedVoice = "tenors"
  Him
  Give my __ _ heart.
}

tenorMusic = \relative c' {
  \partial 4 c4 |
  \slurDotted c4.( c8) c4 f, |
  a( a) a s |
  g4.( d8) d4 f |
  \slurSolid f2( e4) f |
  
  \slurDotted f4.( c'8) c4 f, |
  a4.( a8) a4 a |
  d( d) bes4. a8 |
  a2. a4 |
  
  bes4. c8 bes4( a) |
  bes( bes) a a |
  f( c') bes( a) |
  g2. f4 |
  
  f4.( c'8) c4 f, |
  a2 a4 s |
  \slurSolid d2 bes4.( a8) |
  a1 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \partial 4 f4 |
  \slurDotted f4.( g8) a4 f |
  d( d) d d\rest |
  bes4.( c8) bes4 a |
  \slurSolid g2( c4) f |
  
  \slurDotted f4.( g8) a4 f |
  d4.( d8) d4 d |
  bes( bes) c4. f8 |
  f2. f4 |
  
  d4. f8 d4( c) |
  bes( bes) d d |
  a( a) bes( bes) |
  c2. f4 |
  
  f4.( g8) a4 f |
  d2 d4 d\rest |
  \slurSolid bes2 c4.( f8) |
  f1 \bar "|."
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.9))  } \lyricsto "sopranos" \altoWords
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
