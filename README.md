<!--

@license Apache-2.0

Copyright (c) 2020 The Stdlib Authors.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-->


<details>
  <summary>
    About stdlib...
  </summary>
  <p>We believe in a future in which the web is a preferred environment for numerical computation. To help realize this future, we've built stdlib. stdlib is a standard library, with an emphasis on numerical and scientific computation, written in JavaScript (and C) for execution in browsers and in Node.js.</p>
  <p>The library is fully decomposable, being architected in such a way that you can swap out and mix and match APIs and functionality to cater to your exact preferences and use cases.</p>
  <p>When you use stdlib, you can be absolutely certain that you are using the most thorough, rigorous, well-written, studied, documented, tested, measured, and high-quality code out there.</p>
  <p>To join us in bringing numerical computing to the web, get started by checking us out on <a href="https://github.com/stdlib-js/stdlib">GitHub</a>, and please consider <a href="https://opencollective.com/stdlib">financially supporting stdlib</a>. We greatly appreciate your continued support!</p>
</details>

# Wilcoxon Signed Rank Test

[![NPM version][npm-image]][npm-url] [![Build Status][test-image]][test-url] [![Coverage Status][coverage-image]][coverage-url] <!-- [![dependencies][dependencies-image]][dependencies-url] -->

> One-sample and paired Wilcoxon signed rank test.



<section class="usage">

## Usage

To use in Observable,

```javascript
wilcoxon = require( 'https://cdn.jsdelivr.net/gh/stdlib-js/stats-wilcoxon@v0.2.2-umd/browser.js' )
```

To vendor stdlib functionality and avoid installing dependency trees for Node.js, you can use the UMD server build:

```javascript
var wilcoxon = require( 'path/to/vendor/umd/stats-wilcoxon/index.js' )
```

To include the bundle in a webpage,

```html
<script type="text/javascript" src="https://cdn.jsdelivr.net/gh/stdlib-js/stats-wilcoxon@v0.2.2-umd/browser.js"></script>
```

If no recognized module system is present, access bundle contents via the global scope:

```html
<script type="text/javascript">
(function () {
    window.wilcoxon;
})();
</script>
```

#### wilcoxon( x\[, y]\[, opts] )

Performs a one-sample t-test for the null hypothesis that the data in [array][mdn-array] or [typed array][mdn-typed-array] `x` is drawn from a distribution that is symmetric around zero (i.e., with median zero).

```javascript
// Differences in plant heights, see Cureton (1967)
var x = [ 6, 8, 14, 16, 23, 24, 28, 29, 41, -48, 49, 56, 60, -67, 75 ];
var out = wilcoxon( x );
/* e.g., returns
    {
        'rejected': true,
        'alpha': 0.05,
        'pValue': 0.04125976562499978,
        'statistic': 96,
        // ...
    }
*/
```

When [array][mdn-array] or [typed array][mdn-typed-array] `y` is supplied, the function tests whether the paired differences `x - y` come from a distribution that is symmetric around zero (i.e., with median zero).

```javascript
// Patient measurements at first (x) and second (y) visit, see Hollander & Wolfe (1973)
var x = [ 1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30 ];
var y = [ 0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29 ];

var out = wilcoxon( x, y );
/* e.g., returns
    {
        'rejected': true,
        'alpha': 0.05,
        'pValue': 0.0390625,
        'statistic': 40,
        // ...
    }
*/
```

The returned object comes with a `.print()` method which when invoked will print a formatted output of the hypothesis test results. `print` accepts a `digits` option that controls the number of decimal digits displayed for the outputs and a `decision` option, which when set to `false` will hide the test decision.

<!-- run-disable -->

```javascript
console.log( out.print() );
/* e.g., =>
    Paired Wilcoxon signed rank test

    Alternative hypothesis: Median of the difference `x - y` is not equal to 0

        pValue: 0.0391
        statistic: 40

    Test Decision: Reject null in favor of alternative at 5% significance level
*/
```

The `wilcoxon` function accepts the following `options`:

-   **alpha**: `number` in the interval `[0,1]` giving the significance level of the hypothesis test. Default: `0.05`.
-   **alternative**: Either `two-sided`, `less` or `greater`. Indicates whether the alternative hypothesis is that the mean of `x` is larger than `mu` (`greater`), smaller than `mu` (`less`), or equal to `mu` (`two-sided`). Default: `two-sided`.
-   **correction**: continuity correction adjusting the Wilcoxon rank statistic by 0.5 towards the mean when using the normal approximation. Default: `true`.
-   **exact**: Determines whether to force use of the exact distribution instead of a normal approximation when there are more than fifty data points. Default: `false`.
-   **mu**: `number` denoting the hypothesized median under the null hypothesis. Default: `0`.
-   **zeroMethod**:  Method governing how zero-differences are handled (`pratt`, `wilcox`, or `zsplit`). Default: `'wilcox'`.

By default, the hypothesis test is carried out at a significance level of `0.05`. To choose a different significance level, set the `alpha` option.

```javascript
var table;
var out;
var arr;

arr = [ 2, 4, 3, 1, 0 ];
out = wilcoxon( arr, {
    'alpha': 0.01
});
table = out.print();
/* e.g., returns
    One-Sample Wilcoxon signed rank test

    Alternative hypothesis: Median of `x` is not equal to 0

        pValue: 0.035
        statistic: 21

    Test Decision: Reject null in favor of alternative at 5% significance level
*/

out = wilcoxon( arr, {
    'alpha': 0.1
});
table = out.print();
/* e.g., returns
    One-Sample Wilcoxon signed rank test

    Alternative hypothesis: Median of `x` is not equal to 0

        pValue: 0.035
        statistic: 21

    Test Decision: Fail to reject null in favor of alternative at 1% significance level
*/
```

To test whether the data comes from a distribution with a median different than zero, set the `mu` option.

```javascript
var arr = [ 4, 4, 6, 6, 5 ];
var out = wilcoxon( arr, {
    'mu': 5
});
/* e.g., returns
{
    'rejected': false,
    'pValue': 1,
    'statistic': 0,
    // ...
}
*/
```

By default, a two-sided test is performed. To perform either of the one-sided tests, set the `alternative` option to `less` or `greater`.

```javascript
var arr = [ 4, 4, 6, 6, 5 ];
var out = wilcoxon( arr, {
    'alternative': 'less'
});
var table = out.print();
/* e.g., returns
    One-Sample Wilcoxon signed rank test

    Alternative hypothesis: Median of `x` is less than 0

        pValue: 0.9853
        statistic: 15

    Test Decision: Fail to reject null in favor of alternative at 5% significance level
*/

out = wilcoxon( arr, {
    'alternative': 'greater'
});
table = out.print();
/* e.g., returns
    One-Sample Wilcoxon signed rank test

    Alternative hypothesis: Median of `x` is greater than 0

        pValue: 0.0284
        statistic: 15

    Test Decision: Reject null in favor of alternative at 5% significance level
*/
```

By default, all zero-differences are discarded before calculating the ranks. Set `zeroMethod` to `pratt` when you wish differences of zero to be used in the rank calculation but then drop them or to `zsplit` when differences of zero are shall be used in the ranking procedure and the ranks then split between positive and negative ones. 

```javascript
var arr = [ 0, 2, 3, -1, -4, 0, 0, 8, 9 ];
var out = wilcoxon( arr, {
    'zeroMethod': 'pratt'
});
/* e.g., returns
    {
        'rejected': false,
        'alpha': 0.05,
        'pValue': ~0.331,
        'statistic': 28,
        // ...
    }
*/

out = wilcoxon( arr, {
    'zeroMethod': 'zsplit'
});
/* e.g., returns
    {
        'rejected': false,
        'alpha': 0.05,
        'pValue': ~0.342,
        'statistic': 31,
        // ...
    }
*/
```

By default, the test uses the exact distribution of the rank statistic to calculate the critical values for the test in case of no ties and no zero-differences. Since it is more computationally efficient, starting with fifty observations a normal approximation is employed. If you would like the test to use the correct distribution even for larger samples, set the `exact` option to `true`.

```javascript
var normal = require( '@stdlib/random-base-normal' ).factory;
var rnorm;
var arr;
var out;
var i;

rnorm = normal( 0.0, 4.0, {
    'seed': 100
});
arr = new Array( 100 );
for ( i = 0; i < arr.length; i++ ) {
    arr[ i ] = rnorm();
}

out = wilcoxon( arr, {
    'exact': false
});
/* e.g., returns
    {
        'rejected': false,
        'alpha': 0.05,
        'pValue': ~0.422,
        'statistic': 2291,
        // ...
    }
*/

out = wilcoxon( arr, {
    'exact': true
});
/* e.g., returns
    {
        'rejected': false,
        'alpha': 0.05,
        'pValue': ~0.424,
        'statistic': 2291,
        // ...
    }
*/
```

By default, when using the normal approximation, the test uses a continuity correction, which adjusts the Wilcoxon rank statistic by `0.5` towards the mean. To disable this correction, set `correction` to `false`.

```javascript
var normal = require( '@stdlib/random-base-normal' ).factory;
var rnorm;
var arr;
var out;
var i;

rnorm = normal( 0.0, 4.0, {
    'seed': 100
});
arr = new Array( 100 );
for ( i = 0; i < arr.length; i++ ) {
    arr[ i ] = rnorm();
}

out = wilcoxon( arr, {
    'correction': false
});
/* e.g., returns
    {
        'rejected': false,
        'alpha': 0.05,
        'pValue': ~0.421,
        'statistic': 2291,
        // ...
    }
*/

out = wilcoxon( arr, {
    'correction': true
});
/* e.g., returns
    {
        'rejected': false,
        'alpha': 0.05,
        'pValue': ~0.422,
        'statistic': 2291,
        // ...
    }
*/
```

</section>

<!-- /.usage -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```html
<!DOCTYPE html>
<html lang="en">
<body>
<script type="text/javascript" src="https://cdn.jsdelivr.net/gh/stdlib-js/random-base-discrete-uniform@umd/browser.js"></script>
<script type="text/javascript">
(function () {.factory;
var wilcoxon = require( '@stdlib/stats-wilcoxon' );

var table;
var runif;
var arr;
var out;
var i;

runif = uniform( -50.0, 50.0, {
    'seed': 37827
});
arr = new Array( 100 );
for ( i = 0; i < arr.length; i++ ) {
    arr[ i ] = runif();
}

// Test whether distribution is symmetric around zero:
out = wilcoxon( arr );
table = out.print();
/* e.g., returns
    One-Sample Wilcoxon signed rank test

    Alternative hypothesis: Median of `x` is not equal to 0

        pValue: 0.7714
        statistic: 2438.5

    Test Decision: Fail to reject null in favor of alternative at 5% significance level
*/

// Test whether distribution has median of five:
out = wilcoxon( arr, {
    'mu': 5.0
});
table = out.print();
/* e.g, returns
    One-Sample Wilcoxon signed rank test

    Alternative hypothesis: Median of `x` is not equal to 5

        pValue: 0.0529
        statistic: 1961.5

    Test Decision: Fail to reject null in favor of alternative at 5% significance level
*/

})();
</script>
</body>
</html>
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

* * *

## See Also

-   <span class="package-name">[`@stdlib/stats-ttest`][@stdlib/stats/ttest]</span><span class="delimiter">: </span><span class="description">one-sample and paired Student's t-Test.</span>
-   <span class="package-name">[`@stdlib/stats-ztest`][@stdlib/stats/ztest]</span><span class="delimiter">: </span><span class="description">one-sample and paired z-Test.</span>

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->


<section class="main-repo" >

* * *

## Notice

This package is part of [stdlib][stdlib], a standard library for JavaScript and Node.js, with an emphasis on numerical and scientific computing. The library provides a collection of robust, high performance libraries for mathematics, statistics, streams, utilities, and more.

For more information on the project, filing bug reports and feature requests, and guidance on how to develop [stdlib][stdlib], see the main project [repository][stdlib].

#### Community

[![Chat][chat-image]][chat-url]

---

## License

See [LICENSE][stdlib-license].


## Copyright

Copyright &copy; 2016-2024. The Stdlib [Authors][stdlib-authors].

</section>

<!-- /.stdlib -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[npm-image]: http://img.shields.io/npm/v/@stdlib/stats-wilcoxon.svg
[npm-url]: https://npmjs.org/package/@stdlib/stats-wilcoxon

[test-image]: https://github.com/stdlib-js/stats-wilcoxon/actions/workflows/test.yml/badge.svg?branch=v0.2.2
[test-url]: https://github.com/stdlib-js/stats-wilcoxon/actions/workflows/test.yml?query=branch:v0.2.2

[coverage-image]: https://img.shields.io/codecov/c/github/stdlib-js/stats-wilcoxon/main.svg
[coverage-url]: https://codecov.io/github/stdlib-js/stats-wilcoxon?branch=main

<!--

[dependencies-image]: https://img.shields.io/david/stdlib-js/stats-wilcoxon.svg
[dependencies-url]: https://david-dm.org/stdlib-js/stats-wilcoxon/main

-->

[chat-image]: https://img.shields.io/gitter/room/stdlib-js/stdlib.svg
[chat-url]: https://app.gitter.im/#/room/#stdlib-js_stdlib:gitter.im

[stdlib]: https://github.com/stdlib-js/stdlib

[stdlib-authors]: https://github.com/stdlib-js/stdlib/graphs/contributors

[umd]: https://github.com/umdjs/umd
[es-module]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules

[deno-url]: https://github.com/stdlib-js/stats-wilcoxon/tree/deno
[deno-readme]: https://github.com/stdlib-js/stats-wilcoxon/blob/deno/README.md
[umd-url]: https://github.com/stdlib-js/stats-wilcoxon/tree/umd
[umd-readme]: https://github.com/stdlib-js/stats-wilcoxon/blob/umd/README.md
[esm-url]: https://github.com/stdlib-js/stats-wilcoxon/tree/esm
[esm-readme]: https://github.com/stdlib-js/stats-wilcoxon/blob/esm/README.md
[branches-url]: https://github.com/stdlib-js/stats-wilcoxon/blob/main/branches.md

[stdlib-license]: https://raw.githubusercontent.com/stdlib-js/stats-wilcoxon/main/LICENSE

[mdn-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Typed_arrays

<!-- <related-links> -->

[@stdlib/stats/ttest]: https://github.com/stdlib-js/stats-ttest/tree/umd

[@stdlib/stats/ztest]: https://github.com/stdlib-js/stats-ztest/tree/umd

<!-- </related-links> -->

</section>

<!-- /.links -->
