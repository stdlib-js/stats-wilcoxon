// Copyright (c) 2024 The Stdlib Authors. License is Apache-2.0: http://www.apache.org/licenses/LICENSE-2.0
/// <reference types="./index.d.ts" />
import{primitives as e}from"https://cdn.jsdelivr.net/gh/stdlib-js/assert-is-number-array@v0.2.2-esm/index.mjs";import t from"https://cdn.jsdelivr.net/gh/stdlib-js/assert-is-typed-array-like@v0.2.2-esm/index.mjs";import r from"https://cdn.jsdelivr.net/gh/stdlib-js/utils-define-read-only-property@v0.2.2-esm/index.mjs";import s from"https://cdn.jsdelivr.net/gh/stdlib-js/assert-is-plain-object@v0.2.2-esm/index.mjs";import i from"https://cdn.jsdelivr.net/gh/stdlib-js/stats-ranks@v0.2.2-esm/index.mjs";import{factory as n}from"https://cdn.jsdelivr.net/gh/stdlib-js/stats-base-dists-normal-cdf@v0.2.2-esm/index.mjs";import o from"https://cdn.jsdelivr.net/gh/stdlib-js/stats-base-dists-signrank-cdf@v0.2.2-esm/index.mjs";import a from"https://cdn.jsdelivr.net/gh/stdlib-js/utils-tabulate@v0.2.2-esm/index.mjs";import l from"https://cdn.jsdelivr.net/gh/stdlib-js/math-base-special-signum@v0.2.2-esm/index.mjs";import d from"https://cdn.jsdelivr.net/gh/stdlib-js/math-base-special-sqrt@v0.2.2-esm/index.mjs";import h from"https://cdn.jsdelivr.net/gh/stdlib-js/math-base-special-abs@v0.2.2-esm/index.mjs";import m from"https://cdn.jsdelivr.net/gh/stdlib-js/array-float64@v0.2.2-esm/index.mjs";import p from"https://cdn.jsdelivr.net/gh/stdlib-js/error-tools-fmtprodmsg@v0.2.2-esm/index.mjs";import c from"https://cdn.jsdelivr.net/gh/stdlib-js/assert-contains@v0.2.2-esm/index.mjs";import{isPrimitive as f}from"https://cdn.jsdelivr.net/gh/stdlib-js/assert-is-boolean@v0.2.2-esm/index.mjs";import{isPrimitive as v}from"https://cdn.jsdelivr.net/gh/stdlib-js/assert-is-number@v0.2.2-esm/index.mjs";import{isPrimitive as j}from"https://cdn.jsdelivr.net/gh/stdlib-js/assert-is-string@v0.2.2-esm/index.mjs";import g from"https://cdn.jsdelivr.net/gh/stdlib-js/assert-is-nan@v0.2.2-esm/index.mjs";import x from"https://cdn.jsdelivr.net/gh/stdlib-js/assert-has-own-property@v0.2.2-esm/index.mjs";import u from"https://cdn.jsdelivr.net/gh/stdlib-js/assert-is-positive-integer@v0.2.2-esm/index.mjs";import w from"https://cdn.jsdelivr.net/gh/stdlib-js/math-base-special-roundn@v0.2.2-esm/index.mjs";var b=["two-sided","less","greater"],y=["pratt","wilcox","zsplit"];function E(e,t){return e-t}function L(e){var t,r,i;if(r=4,t=!0,arguments.length>0){if(!s(e))throw new TypeError(p("1Lx3L",e));if(x(e,"digits")){if(!u(e.digits))throw new TypeError(p("1Lx3P","digits",e.digits));r=e.digits}if(x(e,"decision")){if(!f(e.decision))throw new TypeError(p("1Lx2o","decision",e.decision));t=e.decision}}switch(i="",i+=this.method,i+="\n\n",i+="Alternative hypothesis: ","Paired Wilcoxon signed rank test"===this.method?i+="Median of the difference `x - y` is ":i+="Median of `x` is ",this.alternative){case"less":i+="less than ";break;case"greater":i+="greater than ";break;default:i+="not equal to "}return i+=this.nullValue,i+="\n\n",i+="    pValue: "+w(this.pValue,-r)+"\n",i+="    statistic: "+w(this.statistic,-r)+"\n",i+="\n",t&&(i+="Test Decision: ",this.rejected?i+="Reject null in favor of alternative at "+100*this.alpha+"% significance level":i+="Fail to reject null in favor of alternative at "+100*this.alpha+"% significance level",i+="\n"),i}var T=n(0,1);function z(){var n,u,w,z,M,k,P,V,W,A,R,S,q,D,F,J,O,B,C,G,H,I,K,N,Q,U,X,Y,Z;if(!t(Y=arguments[0])&&!e(Y))throw new TypeError(p("1Lx8R",Y));if(J=Y.length,arguments.length>1)if(s(arguments[1]))w=arguments[1];else{if(!t(Z=arguments[1])&&!e(Z))throw new TypeError(p("1LxA5","y",Z));if(J!==Z.length)throw new Error(p("1Lx1E"));arguments.length>2&&(w=arguments[2])}if(S={},w&&(F=function(e,t){if(!s(t))return new TypeError(p("1Lx2V",t));if(x(t,"alpha")){if(e.alpha=t.alpha,!v(e.alpha)||g(e.alpha))return new TypeError(p("1Lx8P","alpha",e.alpha));if(e.alpha<0||e.alpha>1)return new RangeError(p("1Lx8V","alpha",e.alpha))}if(x(t,"alternative")){if(e.alternative=t.alternative,!j(e.alternative))return new TypeError(p("1Lx2W","alternative",e.alternative));if(!c(b,e.alternative))return new Error(p("1Lx4S","alternative",b.join('", "'),e.alternative))}if(x(t,"correction")&&(e.correction=t.correction,!f(e.correction)||g(e.correction)))return new TypeError(p("1Lx2o","correction",e.alpha));if(x(t,"exact")&&(e.exact=t.exact,!f(e.exact)||g(e.exact)))return new TypeError(p("1Lx2o","exact",e.alpha));if(x(t,"mu")&&(e.mu=t.mu,!v(e.mu)||g(e.mu)))return new TypeError(p("1Lx8P","mu",e.mu));if(x(t,"zeroMethod")){if(e.zeroMethod=t.zeroMethod,!j(e.zeroMethod))return new TypeError(p("1Lx2W","zeroMethod",e.alternative));if(!c(y,e.zeroMethod))return new Error(p("1Lx4S","zeroMethod",y.join('", "'),e.zeroMethod))}return null}(S,w),F))throw F;if(G=S.mu||0,n=void 0===S.correction||S.correction,A=void 0===S.alpha?.05:S.alpha,J<2)throw new Error(p("1LxA2",Y));if(D=S.alternative||"two-sided","wilcox"===(u=S.zeroMethod||"wilcox")){if(K=[],Z)for(N=0;N<J;N++)0!==(X=Y[N]-Z[N]-G)&&K.push(X);else for(N=0;N<J;N++)0!==Y[N]&&K.push(Y[N]-G);V=Y.length-K.length}else if(K=new m(J),V=0,Z)for(N=0;N<J;N++)K[N]=Y[N]-Z[N]-G,0===K[N]&&(V+=1);else for(N=0;N<J;N++)K[N]=Y[N]-G,0===K[N]&&(V+=1);if(V===J)throw new Error(p("1Lx1J"));for(J=K.length,C=new m(J),N=0;N<J;N++)C[N]=h(K[N]);for(Q=i(C),P=0,W=0,N=0;N<J;N++)K[N]>0?P+=Q[N]:0===K[N]&&(W+=Q[N]);if(z=function(e){var t,r,s,i;for((e=e.slice()).sort(E),t=e.length,s=1,i=0;s<t;s++)r=e[s],e[i]!==r&&(e[i+=1]=r);return e.length=i+1,e}(Q).length!==Q.length,"zsplit"===u&&(P+=W/2),U=P,H=J*(J+1)*.25,I=J*(J+1)*(2*J+1),"pratt"===u){for(O=[],N=0;N<J;N++)0!==K[N]&&O.push(Q[N]);Q=O,H-=V*(V+1)*.25,I-=V*(V+1)*(2*V+1)}for(M=a(Q),k=0,N=0;N<M.length;N++)M[N][1]>1&&(k+=(X=M[N][1])*(X*X-1));if(k>0&&(I-=.5*k),I=d(I/24),J>50&&!S.exact||V>0||z){if(K=0,n)switch(D){case"two-sided":K=.5*l(U-H);break;case"less":K=-.5;break;default:K=.5}q=(U-H-K)/I,R="two-sided"===D?2*(1-T(h(q))):"greater"===D?1-T(q):T(q)}else q=U,R="two-sided"===D?q>J*(J+1)/4?2*(1-o(q-1,J)):2*o(q,J):"greater"===D?1-o(q-1,J):o(q,J);return r(B={},"rejected",R<=A),r(B,"alpha",A),r(B,"pValue",R),r(B,"statistic",U),r(B,"nullValue",G),r(B,"alternative",D),r(B,"method",(Z?"Paired":"One-Sample")+" Wilcoxon signed rank test"),r(B,"print",L),B}export{z as default};
//# sourceMappingURL=index.mjs.map
