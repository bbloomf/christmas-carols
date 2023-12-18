\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Ding Dong Merrily on High"}}
  poet = \markup\oldStyleNum"George Ratcliffe Woodward (1848–1934)"
  composer = \markup\oldStyleNum"16th century French melody"
  arranger = \markup\oldStyleNum"Arranged by Charles Wood (1866–1926)"
  
  tagline = \markup \concat{ "from " \italic "The Cambridge Carol Book" \oldStyleNum", 1924"}

}
\paper {
  %print-all-headers = ##t
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
  first-page-number = #032
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
  \key bes \major
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c'' {
  \repeat unfold 2 {
    bes4 bes c8 bes a g |
    f2. f4 |
    g bes bes a |
    bes2 bes
  } \break
  \repeat volta 2 {
    f'4.( ees8[ d ees f d] |
    ees4. d8[ c d ees c] |
    d4. c8[ bes c d bes] |
    c4. bes8[ a bes c a] |
    
    bes4. a8[ g a bes g] |
    a4.) g8 f4 f |
    g bes bes a |
    bes2 bes 2
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \repeat unfold 2 {
    f4 f g8 g ees ees |
    c2. f4 |
    f ees c f |
    f2 f |
  }
  \repeat volta 2 {
    r4 f2( bes4 |
    bes8[ a g f] g[ f] ees4) |
    r4  f8([ ees] d4 g |
    g8[ f ees d] ees[ d] c4) |
    
    r4 d8([ c] bes4 ees |
    c8[ d ees]) d c4 f |
    f ees c f |
    f2 f
  }
}
altoWords = \lyricmode {
  \dropLyricsVII
  \set stanza = #"1. "
  Ding dong! mer -- ri -- ly on high in heav’n the bells are ring -- ing:
  Ding dong!  Ve -- ri -- ly the sky is riv’n with an -- gel sing -- ing.
  \markup\italic Gló -- \markup\italic ri -- \markup\italic a, \markup\italic ho -- \markup\italic sán -- \markup\italic na \markup\italic in \markup\italic ex -- \markup\italic cél -- \markup\italic sis!
}
altoWordsII = \lyricmode {
  \dropLyricsVII
  \set stanza = #"2. "
  E’en so here be -- low, be -- low, let stee -- ple bells be swung -- en.
  And i -- o, i -- o, i -- o by priest and peo -- ple sung -- en.
}
altoWordsIII = \lyricmode {
  \dropLyricsVII
  \set stanza = #"3. "
  Pray ye du -- ti -- ful -- ly prime your ma -- tin chime, ye ring -- ers;
  may ye beau -- ti -- ful -- ly rime your eve -- time song, ye sing -- ers.
}

tenorMusic = \relative c' {
  \repeat unfold 2 {
    d4 bes g8 g c bes |
    a2. bes4 |
    bes bes c c |
    d2 d |
  }
  \repeat volta 2 {
    d,4\rest c'( d bes |
    c bes2 c4) |
    d,4\rest a'( bes g |
    a g2 a4) |
    
    d,\rest f( g2 |
    c4) bes c d |
    bes bes c c |
    d2 d
  }
}
tenorWords = \lyricmode {
  \repeat unfold 28 { \skip 1}
  \markup\italic Gló -- _ _ \markup\italic ri -- \markup\italic a, \markup\italic ho -- \markup\italic sán -- \markup\italic na \markup\italic in \markup\italic ex -- \markup\italic cél -- \markup\italic sis!
}

bassMusic = \relative c {
  \repeat unfold 2 {
    bes4 d ees8 ees c c |
    f2. d4 |
    ees g f f |
    bes,2 bes |
  }
  \repeat volta 2 {
    s4 a'4( bes d, |
    c d ees8[ d] c4) |
    s4 f( g bes, |
    a bes c8[ bes] a4) |
    
    s4 d( ees g |
    f) g a bes |
    ees, g f f |
    bes,2 bes
  }
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men"  } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" } \lyricsto "basses" \bassWords
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
