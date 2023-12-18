\version "2.24.0"
\include "util.ly"
\header {
    tagline = \markup { "from" \italic {ChristmasCarolMusic.org}}
}
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
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
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #163
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
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
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
  Quem pas -- tó -- res lau -- da -- vé -- re,
  Qui -- bus án -- ge -- li di -- xé -- re,
  “Ab -- sit vo -- bis jam ti -- mé -- 
  re,
  Na -- \unset associatedVoice
  tus est Rex gló -- ri -- æ,
  Rex gló -- ri -- æ.”
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  Ad quem ma -- gi am -- bu -- lá -- bant,
  Au -- rum, thus, myr -- rham por -- tá -- bant,
  Im -- mo -- lá -- bant hæc sin -- cé -- 
  re
  Le -- \unset associatedVoice
  ó -- ni vic -- tó -- ri -- æ,
  vic -- tó -- ri -- æ.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Ex -- ul -- té -- mus cum Ma -- rí -- a
  In cæ -- lés -- ti hie -- rár -- chi -- a
  Na -- tum pro -- mat vo -- ce pi -- 
  a
  Laus, \unset associatedVoice
  ho -- nor et gló -- ri -- a,
  et gló -- ri -- a.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  Chris -- to re -- gi, De -- o na -- to,
  Per Ma -- rí -- am no -- bis da -- to,
  Mé -- ri -- to ré -- so -- net ve -- 
  re
  Dul -- \unset associatedVoice
  ci cum me -- ló -- di -- a,
  me -- ló -- di -- a.
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "sopranos" \altoWords
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
      \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/2)
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/2)
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Quem Pastores"}}
    poet = \markup\oldStyleNum"Anonymous, 14th Century"
    composer = \markup\oldStyleNum"14th Century German"
    arranger = \markup\oldStyleNum"Arranged by Rev. J.R. Lunn, B.D."
  }
}
\markup \fill-line {\center-column{
  \concat{ "Music from " \italic "The Cowley Carol Book" \oldStyleNum", 1919, Words from " \italic "HymnsAndCarolsOfChristmas.com"}}}
\markup\vspace#1



















global = {
  \key f \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  f2 a4 |
  c2 a4 |
  bes( c) d |
  c2 g4 |
  a( bes) c |
  
  bes( a) g |
  f2 d4 |
  e2 c4 |
  a'2 bes4 |
  c2 d4 |
  
  c2 g4 |
  a2 f4 |
  bes2 bes4 |
  a( g) f |
  f( d) e |
  f2. \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f2 f4 |
  g2 f4 |
  f2 f4 |
  f2 e4 |
  f2 f4 |
  
  d2 e4 |
  d2 b4 |
  c2 c4 |
  c2 f4 |
  f2 f4 |
  
  f2 e4 |
  e2 d4 |
  d2 g4 |
  e2 d4 |
  d2 c4 |
  c2. \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  Quem pas -- tó -- res lau -- da -- vé -- re,
  Qui -- bus án -- ge -- li di -- xé -- re,
  “Ab -- sit vo -- bis jam ti -- mé -- re,
  Na -- tus est rex gló -- ri -- æ.”
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  Ad quem ma -- gi am -- bu -- lá -- bant,
  Au -- rum, thus, myr -- rham por -- tá -- bant,
  Im -- mo -- lá -- bant hæc sin -- cé -- re
  Le -- ó -- ni vic -- tó -- ri -- æ.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Ex -- ul -- té -- mus cum Ma -- rí -- a
  In cæ -- lés -- ti hie -- rár -- chi -- a
  Na -- tum pro -- mat vo -- ce pi -- a
  Laus, ho -- nor et gló -- ri -- a.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  Chris -- to re -- gi, De -- o na -- to,
  Per Ma -- rí -- am no -- bis da -- to,
  Mé -- ri -- to ré -- so -- net ve -- re
  Dul -- ci cum me -- ló -- di -- a.
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
  bes2 bes4 |
  g( a) bes |
  a2 a4 |
  
  bes2 bes4 |
  a2 g4 |
  g2 e4 |
  f2 f4 |
  f2 bes4 |
  
  g( a) bes |
  a2 a4 |
  bes2 d4 |
  c2 a4 |
  bes2 bes4 |
  a2. \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f2 f4 |
  e2 f4 |
  d2 bes4 |
  c2 c4 |
  f2 a4 |
  
  g2 c,4 |
  d2 g4 |
  c,2 c4 |
  f2 d4 |
  a2 bes4 |
  
  c2 c4 |
  cis2 d4 |
  g,2 g4 |
  a2 d4 |
  g,2 c4 |
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWords
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Quem Pastores"}}
%    poet = \markup\oldStyleNum"Anonymous, 14th Century"
%    composer = \markup\oldStyleNum"14th Century German"
    composer = \markup\oldStyleNum"Arranged by Ralph Vaughan Williams (1872–1958)"
    tagline = \markup { "from" \italic {ChristmasCarolMusic.org}}
  }
}

