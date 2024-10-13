\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Once in Royal David’s City"}}
  poet = \markup\oldStyleNum"Cecil Frances Alexander (1818–1895)"
  composer = \markup\oldStyleNum"Henry J. Gauntlett (1805–1876)"
  tagline = \markup { "from" \concat{\italic "Christmas Carols and Hymns for School and Choir" \oldStyleNum", 1910"}}
}
\paper {
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -0.35)
  %     (stretchability . 100))
%{IF_LESSER
  markup-system-spacing #'stretchability = 50
  top-markup-spacing #'stretchability = 30
  last-bottom-spacing #'stretchability = 60
%}%END_IF_LESSER
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #046
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
  \key g \major
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c' {
  \tempo 4 = 108
  \partial 2 d4 fis |
  g4. g8 g[ fis] g[ a] |
  a4 g g b |
  
  d4. b8 b[ a] g[ fis] |
  g2 \bar""\break d4 fis |
  g4. g8 g[ fis] g[ a] |
  
  a4 g g b |
  d4. b8 b[ a] g[ fis] |
  g2 \bar""\break e'4 e |
  
  d4. g,8 c4 c |
  \slurDotted b4( b) e e |
  \slurSolid d4. b8 b[ a] g[ fis] |
  \partial 2 \slurDotted g4( g) \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 c |
  d4. d8 d4 d8[ fis] |
  fis4 g d g |
  g4. g8 e4 d |
  d2 d4 c |
  
  d4. d8 cis4 cis |
  d8[ c!] b4 d g |
  g4. g8 e4 d |
  d2 g4 g |
  
  g8[ fis g] g g4 fis |
  \slurDotted g4( g) e8[ fis] g[ a] |
  d,[ fis g] d e4 d |
  d( d) \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  Once in roy -- al Da -- _ vid’s _ cit -- y
  Stood a low -- ly cat -- _ tle _ shed,
  Where a moth -- er laid _ her _ Ba -- by
  In a man -- ger for _ His _ bed:
  \set associatedVoice = "tenors"
  Ma -- ry was _ _ that moth -- er mild, _
  Je -- sus Christ _ _ her lit -- _ tle _ Child. _
}
altoWordsII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  He came down to earth _ from _ heav -- en,
  Who is God and Lord _ of _ all,
  And His shel -- ter was _ a _ sta -- ble,
  And His cra -- dle was _ a _ stall;
  \set associatedVoice = "tenors"
  With the poor, _ _ and mean, and low -- ly,
  Lived on earth _ _ our Sav -- _ ior _ ho -- ly.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  And, through all His won -- drous child -- hood,
    He would hon -- or and o -- bey,
  Love, and watch the low -- ly maid -- en
    In whose gen -- tle arms He lay;
  \set associatedVoice = "tenors"
  Chris -- tian chil -- dren all must be __
  Mild, o -- be -- dient, good as He. __
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  Je -- sus is our child -- _ hood’s _ pat -- tern,
  Day by day like us _ He _ grew;
  He was lit -- tle, weak, _ and _ help -- less,
  Tears and smiles, like us, _ He _ knew:
  \set associatedVoice = "tenors"
  And He feel -- _ _ eth for our sad -- ness,
  And He shar -- _ _ eth in _ our _ glad -- ness.
}
altoWordsV = \lyricmode {
  \dropLyricsIX
  \set ignoreMelismata = ##t
  \set stanza = #"5. "
  And our eyes at last _ shall _ see Him,
  Through His own re -- deem -- _ ing _ love;
  For that Child so dear _ and _ gen -- tle
  Is our Lord in heav’n _ a -- _ bove:
  \set associatedVoice = "tenors"
  And He leads _ _ His chil -- dren on __ _
  To the place _ _ where He _ is _ gone. _
}

tenorMusic = \relative c' {
  b4 a |
  g4. b8 b[ a] b[ c] |
  c4 b b g |
  g4. d'8 d[ c] b[ a] |
  b2 b4 a |
  
  g4. b8 g4 g |
  fis g b g |
  g4. d'8 d[ c] b[ a] |
  b2 c4 c |
  
  b8[ c d] b a4 d |
  \slurDotted d4( d) c c |
  c8[ a b] d d[ c] b[ a] |
  b4( b) \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g4 a |
  b4. g8 d'4 d |
  d g g e |
  b4. g8 c4 d |
  g,2 g4 a |
  
  b4. g8 e'4 a, |
  d g g e |
  b4. g8 c4 d |
  g,2 c8[ d] e[ fis] |
  
  g[ a b] g d4 d |  
  \slurDotted g4( g) c,8[ d] e[ fis] |
  g4. g8 c,4 d |
  g,( g) \bar "|."
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
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" } \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWords
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
    \tempo 4 = 108
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
