\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"A Virgin Unspotted"}}
  poet = \markup\oldStyleNum"17th Century English"
  composer = \markup\oldStyleNum"English"
  arranger = \markup\oldStyleNum"Arranged by Sir John Stainer (1840–1901)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  paper-height = 11\in
  paper-width = 8.5\in
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
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #030
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
  \autoBeamOff
  \key f \major
  \time 3/4
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  \repeat volta 4 {
    \partial 4 a8[ bes] |
    c4 d c8[ bes] |
    a4 g c8[ a] |
    bes[ c] d4 c |
    f,2 a8[ bes] |
    c4 d c8[ bes] | \break
    
    a4 g c8[ a] |
    bes[ c] d4 c |
    f,2 f8[ g] |
    a4 g c |
    bes8[ a] g4 d' | \break
    
    c f8[ e] d4 |
    c2 a8[ bes] |
    c4 d c8[ bes] |
    a4 g c8[ a] |
    bes[ c] d4 c |
    f,2 \bar "||" \break
    
    %chorus
    f8\noBeam g8 |
    a4 g c |
    bes8[ a] g4 d' |
    c f8[ e] d4 |
    
    c2 \bar""\break a8[ bes] | 
    c4 d c8[ bes] |
    a4 g c8[ a] |
    bes[ c] d4 c |
    \partial 2 f,2 \break
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f4 |
  f f g |
  f e f |
  f f e |
  f2 f4 |
  f f g |
  
  f e f |
  f f e |
  f2 f4 |
  e e f |
  g8[ f] e4 g |
  
  a a8[ g] f4 |
  e2 f4 |
  f d d |
  f e f |
  f f e |
  f2 \bar "||"
  
  %chorus
  f8\noBeam f8 |
  e[ f] g[ e] f4 |
  f e g |
  a a8[ g] f4 |
  
  e2 f4 |
  f d g |
  f e f |
  f d e |
  f2
}
altoWords = {
    \dropLyricsIX
  \lyricmode {
    \set stanza = #"1. "
    %\set ignoreMelismata = ##t
    %\unset ignoreMelismata
    A __ Vir -- gin un -- spot -- ted, the pro -- phet fore -- told,
    Should bring forth a __ Sav -- ior, which now we be -- hold,
    To __ be our Re -- deem -- er from death, hell __ and sin,
    Which Ad -- am’s trans -- gres -- sion had wrap -- ped us in.
  }
  \set stanza = \markup\dynamic"ff "
  \lyricmode {
    \set associatedVoice = "sopranos"
    Aye 
    \set associatedVoice = "altos"
    and there -- fore
    \unset associatedVoice
    be mer -- ry, set sor -- row a -- side,
    Christ Je -- sus, our Sav -- ior, was born on this tide.

    \set stanza = #"5. "
    The King of all kings  to this world be -- ing brought,
    Small store of fine lin -- en to __ wrap Him was sought,
    But when she had swad -- dled her young Son so sweet,
    With -- in an ox man -- ger she laid __ Him to sleep.
  }
  \set stanza = \markup\dynamic"ff "
  \lyricmode {
    \set associatedVoice = "altos"
    \set associatedVoice = "sopranos"
    Aye 
    \set associatedVoice = "altos"
    and there -- fore
    \unset associatedVoice
    be mer -- ry, set sor -- row a -- side,
    Christ Je -- sus, our Sav -- ior, was born on this tide.
  }
}
altoWordsII = \lyricmode {
    \dropLyricsIX
  \set stanza = #"2. "
  At __ Beth -- le -- hem ci -- ty in Jew -- ry it was
  That Jo -- seph and Ma -- ry to -- geth -- er did pass,
  All __ for to be tax -- ed with ma -- ny __ one moe.
  Great Cae -- sar com -- mand -- ed the same should be so.

  \repeat unfold 23 {\skip1}
  \set stanza = #"6. "
  Then God sent an an -- gel from heav -- en so high,
  To __ cer -- tain poor shep -- herds in __ fields where they lie,
  And bade them no long -- er in sor -- row to stay,
  Be -- cause that our Sav -- ior was born on this day.
}
altoWordsIII = \lyricmode {
    \dropLyricsIX
  \set stanza = #"3. "
  But when they had en -- tered the ci -- ty so fair,
  A __ num -- ber of __ peo -- ple so __ might -- y was there,
  That Jo -- seph and Ma -- ry, whose sub -- stance was small,
  Could find in the inn there no lodg -- ing at all.

  \repeat unfold 23 {\skip1}
  \set stanza = #"7. "
  Then pres -- ent -- ly __ af -- ter the shep -- herds did spy
  Vast num -- bers of __ an -- gels to __ stand in the sky;
  They joy -- ful -- ly talk -- ed and sweet -- ly __ did sing:
  “To God be all glo -- ry, our heav -- en -- ly King.”
}
altoWordsIV = \lyricmode {
    \dropLyricsIX
  \set stanza = #"4. "
  Then were they con -- strain’d in a __ sta -- ble to lie,
  Where hors -- es and ass -- es they used for to tie:
  Their lodg -- ing so sim -- ple they took it __ no scorn,
  But_a -- gainst the next morn -- ing our Sav -- ior was born.

  \repeat unfold 23 {\skip1}
  \set stanza = #"8. "
  To __ teach us hu -- mil -- i -- ty __ all __ this was done,
  And learn we from thence haugh -- ty __ pride for to shun;
  A __ man -- ger His cra -- dle Who came from a -- bove,
  The great God of mer -- cy, of __ peace, and of love.
}
tenorMusic = \relative c' {
  c8[ bes] |
  a4 d d |
  c c c |
  f, bes bes |
  a2 c4 |
  c bes d |
  
  c c c |
  f, bes bes |
  a2 a8[ bes] |
  c4 c c |
  d e e |
  
  f c b |
  c2 c8[ bes] |
  a4 f g |
  a8[ bes] c4 c |
  f, bes bes |
  a2 \bar "||"
  
  %chorus
  a8\noBeam bes |
  c4 d8[ c] c4 |
  d e e |
  f c b |
  
  c2 f,8[ g] |
  a4 f d' |
  c c c |
  f, bes bes |
  a2
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f8[ g] |
  a4 bes g |
  c, c a |
  d bes c |
  f2 f8[ g] |
  a4 bes g |
  
  c, bes a |
  d bes c |
  f2 d4 |
  c bes' a |
  g c bes |
  
  a f g |
  c,2 f4 |
  a, bes g |
  c c a |
  d bes c |
  f2 \bar "||"
  
  %chorus
  d8\noBeam d |
  c4 bes' a |
  g c bes |
  a f g |
  
  c,2 d4 |
  a bes g |
  c c a |
  d bes c |
  f2
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
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global {\sopMusic \sopMusic} >> }
      \new Voice = "altos" { \voiceTwo << \global {\altoMusic \altoMusic} >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" } \lyricsto "sopranos" \sopWords
%    \new Lyrics = "altosVIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVIII
%    \new Lyrics = "altosVII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVII
%    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
%    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.4))} \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global {\tenorMusic \tenorMusic} >> }
      \new Voice = "basses" { \voiceTwo << \global {\bassMusic \bassMusic} >> }
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
}

