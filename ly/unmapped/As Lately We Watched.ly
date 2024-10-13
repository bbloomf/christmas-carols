\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"As Lately We Watched"}}
  poet = \markup\oldStyleNum"Anonymous"
  composer = \markup\oldStyleNum"19th Century Austrian"
  tagline = ""
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
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #085
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
  \key aes \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \partial 4 ees4^\mf |
  aes aes bes |
  aes c8[ bes] aes[ g] |
  f4 bes g |
  
  aes2 \bar""\break ees4 |
  aes4 aes4 bes |
  aes c8[ bes] aes[ g] |
  f4 bes g |
  aes2. | \break
  
  c4 c8[ bes] aes[ g] |
  f2. |
  bes4 bes8[ aes] g[ f] |
  ees2 \bar""\break ees4 |
  
  aes aes bes |
  aes c8[ bes] aes[ g] |
  f4 bes g |
  \partial 2 aes2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  ees4 |
  ees f g |
  ees aes8[ g] ees4 |
  des f ees |
  
  ees2 ees4 |
  c ees ees |
  ees aes8[ g] ees4 |
  des f ees |
  ees2. |
  
  ees4 ees8[ e] f[ ees] |
  des2. |
  d4 f d |
  bes2 ees4 |
  
  ees c ees |
  ees aes8[ g] ees4 |
  des f ees |
  ees2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  As late -- ly we watch’d o’er our fields thro’ the night,
  A star there was seen of __ such glo -- ri -- ous light;
  All thro’ __ the __ night, an -- gels did __ sing,
  In ca -- rols, so sweet, of __ the __ birth of a King.
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
  A King of such beau -- ty __ was ne’er  be -- fore seen,
  And Ma -- ry His moth -- er __ so __ like to a queen.
  Blest be __ the __ hour, wel -- come the __ morn,
  For Christ our dear Sav -- ior __ on __ earth now is born.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  His throne is a man -- ger, __ His court is a loft,
  But troops of bright an -- gels, in __ lays sweet and soft,
  Him they __ pro -- claim, our Christ by __ name,
  And earth, sky and air straight are __ fill’d with His fame.
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  Then shep -- herds, be joy -- ful, __ sa -- lute your liege King,
  Let hills and dales ring to __ the __ song that ye sing,
  Blest be __ the __ hour, wel -- come the __ morn,
  For Christ our dear Sav -- ior __ on __ earth now is born.
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c {
  ees4_\mf |
  c' c ees |
  c ees aes, |
  aes des des |
  
  des( c) g |
  aes aes g8[ des'] |
  c4 ees aes, |
  aes des des |
  des2( c4) |
  
  c4 c c |
  aes2. |
  bes4 bes bes8[ aes] |
  g2 g4 |
  
  aes4 aes g |
  c ees aes, |
  aes des des |
  c2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  ees4 |
  aes8[ g] f4 ees |
  aes aes,8[ bes] c4 |
  des8[ c] bes4 ees |
  
  aes,2 ees'4 |
  f c ees |
  aes aes,8[ bes] c4 |
  des8[ c] bes4 ees |
  aes,2. |
  
  aes'4 aes8[ g] f[ c] |
  des2. |
  bes4 d bes |
  ees2 ees8[ des] |
  
  c4 f ees |
  aes aes,8[ bes] c4 |
  des8[ c] bes4 ees |
  aes,2 \bar "|."
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
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "sopranos" \altoWords
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
    \tempo 4 = 120
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
