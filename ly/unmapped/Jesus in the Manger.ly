\version "2.14.2"
\include "util.ly"
\header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Jesus in the Manger"}}
    poet = \markup\oldStyleNum"Translated by Rev. H.R. Bramley (1833–1917) from Latin"
    composer = \markup\oldStyleNum"Henry Smart (1813–1879)"
    tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
  }\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
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
  first-page-number = #136
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

\markup\fill-line{\concat{"from " \italic "CyberHymnal.org"}}
\markup\vspace#2.8

















%IF_NOT_LESSER
global = {
  \key a \major
  \time 4/4
  \autoBeamOff
}
globalNoTime = {
  \key a \major
  \autoBeamOff
}
sopMusic = \relative c' {
  \once \override Score.RehearsalMark #'self-alignment-X = #LEFT
  \mark \markup\italic "Con spirito."
  \partial 2 e4 e |
  fis e a b |
  cis8[ d] e4 cis b |
  a gis8[ a] b4 fis |
  
  gis4( fis) e e |
  fis e a b |
  cis8[ d] e4 a, b |
  cis b8[ a] b4 gis |
  \partial 2 a2 \bar "||" \break
}
sopMusicII = \relative c' {
  \partial 2 b'2\rest |
  e,4 a a gis |
  a cis fis, gis |
  a cis b gis |
  
  a(^\< fis) e^\f d' |
  d cis b fis' |
  fis e a, b |
  cis a a gis |
  \partial 2. a2.\fermata \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  cis4 cis |
  d e cis e |
  e a gis gis |
  fis e e dis |
  
  e( d) cis b |
  cis cis cis e |
  e g fis fis8[ gis] |
  a4 fis fis e8[ d] |
  cis2 \bar "||"
}
altoMusicII = \relative c' {
  s2 |
  cis4 e e e |
  e a8[ gis] fis4 fis |
  fis a eis eis |
  
  cis( d) b e |
  e e fis gis |
  gis a a fis |
  eis cis fis e8[ d] |
  cis2. \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  Why, Most High -- est, art Thou ly -- ing, In a man -- ger poor and low?
  Thou, the fires of heav’n sup -- ply -- ing, Come a sta -- ble’s cold to know?
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
  On a Moth -- er’s breast Thou sleep -- est,
  Moth -- er, yet a Vir -- gin still;
  Sad, with eyes be -- dimmed Thou weep -- est,
  Eyes, which Heav’n with glad -- ness fill.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  Weak the Strong, of strength the Giv -- er:
  Small, Whose arms cre -- a -- tion span;
  Bound, Who on -- ly can de -- liv -- er;
  Born is He Who ne’er be -- gan.
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
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
  a4 a |
  a a a gis |
  a8[ b] cis4 e d |
  cis cis gis8[ a] b4 |
  
  b2 cis4 gis |
  a gis a gis |
  a8[ b] cis4 d b |
  e d8[ cis] d4 b |
  a2 \bar "||"
}
tenorMusicII = \relative c {
  r2 |
  e4 cis' b b |
  a e' d d |
  cis cis cis b |
  
  a2 d4 gis, |
  a a8[ gis] fis4 b |
  b cis cis d |
  gis, a b b |
  a2.\fermata \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  a4 a |
  d cis fis e |
  a a e e |
  fis cis b b |
  
  e( fis8[ gis]) a4 gis |
  fis cis fis e8[ d] |
  cis4 a d d |
  cis fis d e |
  a,2 \bar "||"
}
bassMusicII = \relative c {
  e4 b' |
  b a e d' |
  d cis cis b |
  a gis8[ fis] gis4 cis, |
  
  fis2 gis4 e |
  s a, d d |
  d cis s d |
  cis s d s |
  e2.\fermata \bar "|."
}
bassIIMusic = \relative c {
  g2\rest |
  a4. cis8 e4 e |
  a, a b b |
  cis cis cis cis |
  
  fis2 gis4 e |
  a s s s |
  s s fis s |
  s fis s e |
  a,2. \bar "|."
}
bassWords = \lyricmode {}
dropLyrics = {
  \override LyricText #'extra-offset = #'(0 . -1.3)
  \override LyricHyphen #'extra-offset = #'(0 . -1.3)
  \override LyricExtender #'extra-offset = #'(0 . -1.3)
  \override StanzaNumber #'extra-offset = #'(0 . -1.3)
}
bassWordsChorus = {
  \dropLyrics
  \set stanza = \markup\dynamic"f "
  \lyricmode {
    O what works of love stu -- pen -- dous,
    Je -- su,
    \set associatedVoice = "tenorsII"
    Were sal -- va -- tion’s price!
    Burn -- ing wert Thou to be -- friend us,
    Ex -- iles far from Pa -- ra -- dise.
  }
}
chorusWordsBass = {
  \lyricmode {
    O what works of love stu -- pen -- dous
    Were sal -- va -- tion’s price!
    Burn -- ing \set associatedVoice = "bassI" wert Thou to be -- friend \unset associatedVoice us,
    \set associatedVoice = "bassI" 
    Ex -- iles \unset associatedVoice far \set associatedVoice = "bassI" from \unset associatedVoice Pa -- ra -- dise.
  }
}
chorusWords = {
  \set stanza = \markup\dynamic"p"
  \lyricmode {
    O what works of love stu -- pen -- dous
    Were sal -- va -- tion’s price!
    Burn -- ing wert Thou to be -- friend us,
    Ex -- iles far from Pa -- ra -- dise.
  }
}
pianoRH = \relative c' {
  s1*8 |
  r2 |
  <cis e>4 <cis e a> <b e a> <d e gis> |
  << \new Voice { \voiceOne <d e a> <cis e cis'> <cis d fis> <b d fis gis> }
     \new Voice { \voiceTwo s4 a'8[ gis] s2 }
  >>
  <a fis cis>4 <cis a cis,> <b eis, cis> <gis eis b> |
  
  <a cis,> <fis d> <e d> <e d'> |
  <e d'> <e cis'> <fis b> <gis fis'> |
  <gis fis'> <a e'> a <fis b> |
  <eis cis'> <cis a'> <fis a> << \new Voice {\voiceOne gis} \new Voice {\voiceTwo e8[ d] } >> |
  <cis a'>2. \bar "|."
}
pianoLH = \relative c' {
  s1*8 |
  \partial 2 e,4 b' |
  << \new Voice { \voiceOne <b e,>4 a e <e b'> }
     \new Voice { \voiceTwo a,4~ a8[ cis] } >> |
  a4 a b b |
  <cis a'> << \new Voice {\voiceTwo cis} \new Voice {\voiceOne gis'8[ fis]}>> <gis cis,>4 cis, |
  
  <fis a>2 <gis b>4 <gis e> |
  a << \new Voice { \voiceOne a8[ gis] } \new Voice {\voiceTwo a,4} >> <d fis>4 <d b'> |
  <d b'> <cis cis'> <fis cis'> <d d'> |
  <cis gis'> <fis a> <d b'> <e b'> |
  <a e a,>2. \bar "|."
}

\score {
  <<
   \new ChoirStaff <<
    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \set Staff.explicitKeySignatureVisibility = #end-of-line-invisible
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \set Staff.explicitKeySignatureVisibility = #end-of-line-invisible
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" } \lyricsto "basses" \bassWords
  >>
   \new ChoirStaff <<
    \new Staff = womenII <<
      \set Staff.explicitKeySignatureVisibility = #end-of-line-invisible
      \new Voice = "sopranosII" { \globalNoTime s1*8 \voiceOne \sopMusicII }
      \new Voice = "altosII" { \globalNoTime s1*8 \voiceTwo \altoMusicII }
    >>
    \new Lyrics \with { alignBelowContext = #"womenII" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranosII" \chorusWords
%    \new Staff = altosII <<
%      \new Voice = "altosII" { \globalNoTime s1*8 \altoMusicII }
%    >>
%    \new Lyrics \with { alignBelowContext = #"altosII" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altosII" \chorusWords
    \new Staff = tenorsII <<
      \set Staff.explicitKeySignatureVisibility = #end-of-line-invisible
      \clef "G_8"
      \new Voice = "tenorsII" { \globalNoTime s1*8 \tenorMusicII }
    >>
%    \new Lyrics \with { alignBelowContext = #"tenorsII" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenorsII" \chorusWords
    \new Staff = bassesII <<
      \set Staff.explicitKeySignatureVisibility = #end-of-line-invisible
      \clef bass
      \new Voice = "bassI" { \globalNoTime \voiceOne s1*8 << \bassMusicII >> }
      \new Voice = "bassII" { \globalNoTime \voiceTwo s1*8 << \bassIIMusic >> }
    >>
    \new Lyrics \with { alignBelowContext = #"tenorsII" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "bassI" \bassWordsChorus
    \new Lyrics \with { alignBelowContext = #"bassesII" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "bassII" \chorusWordsBass
    
   >>
   \new PianoStaff <<
      \new Staff {
        \new Voice { \global \pianoRH }
      }
      \new Staff {
        \clef "bass" \global \pianoLH
      }
   >>
  >>
  \layout {

%6.14 \context {\Lyrics\override LyricText #'font-size = #0.8 }
    #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.5 20)))
    \context {
      \Score
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      %\override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves
      \override VerticalAxisGroup #'remove-first = ##t
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
