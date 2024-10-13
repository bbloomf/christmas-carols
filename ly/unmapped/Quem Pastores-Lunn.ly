\version "2.14.2"
\include "util.ly"
\header {
    tagline = \markup { "from" \italic {ChristmasCarolMusic.org}}

    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Quem Pastores"}}
    poet = \markup\oldStyleNum"Anonymous, 14th Century"
    composer = \markup\oldStyleNum"14th Century German"
    arranger = \markup\oldStyleNum"Arranged by Rev. J.R. Lunn, B.D."
  }
\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -15)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #163
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
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  f2 a4 |
  c2 a4 |
  bes( c) d |
  c g2 |
  a2 c4 |
  
  bes( a) g |
  f2 d4 |
  e4 c2 |
  a'2 bes4 |
  c2 d4 |
  
  c2 g4 |
  a4 f2 |
  bes2 bes4 |
  a( g) c |
  c( a) b |
  c2 f,4 |
  f( d) e |
  f2. \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f2 f4 |
  g2 f4 |
  f2 f4 |
  e4.( f8) g4 |
  f2 f4 f4.( e8) d[ e] |
  
  d4.( c8) b4 |
  c4 c2 |
  c2 d4 |
  e2 g8[ f] |
  e4( f) g |
  f4 d2~ |
  
  d4 g e |
  f( g8[ f]) e4 |
  a4.( g8) f4 |
  e2 c4 |
  d4.( c8) bes4 |
  c2. \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  Quem pas -- to -- res lau -- da -- ve -- re,
  Qui -- bus an -- ge -- li di -- xe -- re,
  “Ab -- sit vo -- bis jam ti -- me -- 
  re,
  Na -- \unset associatedVoice
  tus est Rex glo -- ri -- æ,
  Rex glo -- ri -- æ.”
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  Ad quem ma -- gi am -- bu -- la -- bant,
  Au -- rum, thus, myr -- rham por -- ta -- bant,
  Im -- mo -- la -- bant hæc sin -- ce -- 
  re
  Le -- \unset associatedVoice
  o -- ni vic -- to -- ri -- æ,
  vic -- to -- ri -- æ.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Ex -- ul -- te -- mus cum Ma -- ri -- a
  In cæ -- les -- ti hie -- rar -- chi -- a
  Na -- tum pro -- mat vo -- ce pi -- 
  a
  Laus, \unset associatedVoice
  ho -- nor et glo -- ri -- a,
  et glo -- ri -- a.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  Chris -- to re -- gi, De -- o na -- to,
  Per Ma -- ri -- am no -- bis da -- to,
  Me -- ri -- to re -- so -- net ve -- 
  re
  Dul -- \unset associatedVoice
  ci cum me -- lo -- di -- a,
  me -- lo -- di -- a.
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
  a2 c4 |
  c2 c4 |
  bes( a) g |
  g c2 |
  c4( f4.) ees8 |
  d4( c) bes |
  
  a2 g4 |
  g4.( f8) g4 |
  a( g) f |
  g2 g4 |
  g2 c4 |
  c2 d4 |
  
  d4.( c8) bes4 |
  c2 c4 |
  c2 d4 |
  g,2 a8[ g] |
  f2 g4 |
  a2. \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f2 f4 |
  e2 f4 |
  d( c) bes |
  c4.( d8) e4 |
  f2 a,4 |
  bes( c) c |
  
  d8([ e] f4) g |
  c,4.( d8) e4 |
  f( e) d |
  c2 b4 |
  c( d) e |
  f2 bes4 |
  
  g2 g4 |
  f( e) a8[ g] |
  f4.( e8) d4 |
  c4.( bes8) a4 |
  bes4.( a8) g4 |
  f2. \bar "|."
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
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "sopranos" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 2)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 2)
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
    \tempo 4 = 120
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
