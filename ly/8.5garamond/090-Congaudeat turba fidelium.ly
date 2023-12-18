\version "2.24.0"
\include "util.ly"
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %score-markup-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -11)
  %     (stretchability . 100))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #090
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
  \time 6/4
}

sopMusic = \relative c'' {
  \repeat volta 2 {
    \partial 4 g4 |
    g( a bes a g) f |
    g2.( d'2)  d4 |
    c( d) ees d( c) d |
    bes2.~ bes4 bes\rest \bar""\break bes |
    
    d2 c4 d2 g,4 |
    g( a) c bes( a) f |
    a( bes) c bes( a) f |
    \partial 4*5 g2.~ g2\fermata \break 
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c'' {
  g4 |
  e2( f4 f2) f4 |
  e2.( f2) f4 |
  f2 g4 f2 f4 |
  d2.~ d4 s4 bes' |
  
  f2 f4 f2 d4 |
  e2 f4 f2 f4 |
  f2 f4 f2 f4 |
  e2.( d2)
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  Con -- gáu -- de -- at __ tur -- ba fi -- dé -- li -- um, __
  Vir -- go ma -- ter pé -- pe -- rit fí -- li -- um
  in Béth -- le -- hem. __
  
  
  \set stanza = #"5. "
  In oc -- tá -- va __ dum cir -- cum -- cí -- di -- tur, __
  No -- men e -- i Je -- sus im -- pó -- ni -- tur
  in Béth -- le -- hem. __
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "  
  Ad pas -- tó -- res __ de -- scén -- dit án -- ge -- lus, __
  Di -- cens e -- is: “na -- tus est Dó -- mi -- nus
  in Béth -- le -- hem.” __
  
  
  \set stanza = #"6. "
  Tri -- ni, __ tri -- no, __ tri -- na dant mú -- ne -- ra, __
  Re -- gi re -- gum su -- gén -- ti ú -- be -- ra
  in Béth -- le -- hem. __
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Lo -- que -- bán -- tur __ pas -- tó -- res ín -- vi -- cem, __
  “Trans -- e -- á -- mus ad no -- vum hó -- mi -- nem
  in Béth -- le -- hem.” __
  
  
  \set stanza = #"7. "
  Col -- lý -- ri -- das __ si -- mul cum néc -- ta -- re __
  Be -- ne -- dí -- cat Chris -- tus Rex gló -- ri -- æ
  in Béth -- le -- hem. __
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  Ad præ -- sé -- pe __ stant bos et á -- si -- nus, __
  Co -- gno -- vé -- runt quis es -- set Dó -- mi -- nus
  in Béth -- le -- hem. __
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
altoWordsVII = \lyricmode {
}
tenorMusic = \relative c' {
  g4 |
  c2( d4 c2) a4 |
  c2.( d4 c4) bes |
  a2 bes4 a2 a4 |
  bes2.~ bes4 s4 bes |
  
  bes2 a4 a2 bes4 |
  c2 c4 f,2 a4 |
  d2 c4 d( c) a |
  c2.( b2\fermata)
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  g4 |
  c,2( bes4 f'2) f4 |
  c2.( bes2) bes4 |
  f'2 ees4 f2 f4 |
  bes,2.( bes4) d4\rest bes'4 |
  
  bes2 f4 d2 g4 |
  c,2 a4 d2 d4 |
  d2 a4 bes( f') f |
  c2.( g2)
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
%    \new Lyrics = sopranos { s1 }
    \new Staff = women <<
      \new Voice = "sopranos" {
        \voiceOne
        << \global \repeat unfold2\sopMusic >>
      }
      \new Voice = "altos" {
        \voiceTwo
        << \global \repeat unfold2\altoMusic >>
      }
    >>
    \new Staff = men <<
      \clef bass
      \new Voice = "tenors" {
        \voiceOne
        << \global \repeat unfold2\tenorMusic >>
      }
      \new Voice = "basses" {
        \voiceTwo << \global \repeat unfold2\bassMusic >>
      }
    >>
%    \new Lyrics = basses { s1 }
%    \context Lyrics = sopranos \lyricsto sopranos \sopWords
    \new Lyrics = "altosVII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVII
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "sopranos" \altoWords
    \context Lyrics = tenors \lyricsto tenors \tenorWords
%    \context Lyrics = basses \lyricsto basses \bassWords
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
      % a little smaller so lyrics
      % can be closer to the staff
      \Staff
      \override VerticalAxisGroup.minimum-Y-extent = #'(-3 . 3)
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Congaudeat turba fidelium"}}
    poet = \markup\concat{\italic \oldStyleNum"from an 11th Century Manuscript"}
    composer = \markup\oldStyleNum"Old Melody in Hypo-Dorian Mode"
    arranger = \markup\oldStyleNum"Arranged by G. H. Palmer"
  }
}
\header {
    tagline = \markup\concat{ "Words from " \italic"Piæ Cantiones" \oldStyleNum", 1582"}
  }

