---
layout: post
title: "Img.js - first optimizations"
category: blog
---

This winter I studied the fascinating [course](https://www.coursera.org/course/images)
about image and video processing by Guillermo Shapiro and
decided to make my own javascript library with bells and whistles
and also try a few approaches to code that were not battle tested by me.

From the start I wanted to get several things:

- es6 library code with umd es5 code at the end so that it could be used in any browser environment.
- monadic approach to api, so that all the operations on images can be both synchronous and asynchronous,
  and we still get all this beautiful chaining api.
- internal api, that will consists from the set of operators, that can be composed together before
  applying to the actual image. The rationale is that if you for example make two consequent inverse
  operations than the library should not process image at all in this case. And also it should be very
  fun if I can get that far.
- very little public api that is really public only for transformation plugins. In ideal situation
  I want to be able to build the library for any number of transformations if I want to and don't
  ship everything under the sun just because it's easier. And who nows, may be I will make a img.jspm some day :)

First two goals were relatively easy ones and probably I'll write about them in next post,
but the last one required at least some understanding about what kind of transformations I will want
to make with the image so that I could start defining some solid api.

The first and the most trivial transformation is [identity](https://github.com/can3p/img.js/blob/731b8989a49ca441508018902a7e9752123f6ad7/dist/img.js#L65)
where you transform the image in such a way that the result will be identical
to the source. I got there very easily and decided to move to the next one
after that and use the identity filter only in cases if I'm changing core api
and want to test on something that doesn't contains potential algoritmic errors
for sure.

The next easy filter was grayscale. The first thing to notice there is that there
is no such thing as canonical grayscale. If you check [this](http://www.johndcook.com/blog/2009/08/24/algorithms-convert-color-grayscale/)
post than you will see that there are at least three of them and there should be
more because the only requirement is equality of R, G and B values for each pixel
at the end. I took the path of luminosity approach. I have no idea about where
the coefficients come from, but here is the first naive implementation:

```javascript
lift('grayscale', function grayscale(data) {
  var new_data = create_image_data(data.width, data.height);
  var cur_pos, new_pos;
  var luminosity;

  var arr = data.data;
  var new_arr = new_data.data;

  for (var x = 0; x < data.width; ++x) {
    for (var y = 0; y < data.height; ++y) {
      cur_pos = (y * data.width + x) * 4;
      new_pos = (y * data.width + x) * 4;
      luminosity =  0.21 * arr[cur_pos] +
                    0.72 * arr[cur_pos + 1] +
                    0.07 * arr[cur_pos + 2];

      new_arr[new_pos] = luminosity;
      new_arr[new_pos + 1] = luminosity;
      new_arr[new_pos + 2] = luminosity;
      new_arr[new_pos + 3] = arr[cur_pos + 3];
    }
  }

  return new_data;
});
```

Here is the client code to run this:

```javascript
img('./test.jpg')
  .grayscale()
  .outputTo(document.querySelector('.output'));
```

The facts to mention here is that:

- the code is asynchronous
- I don't care about browser compatability at all
  right now and it was one of the starting design decisions
  for the library.

I've been looking on the grayscale code again and again
and it was apparent that it's nothing more than just
matrix multiplication.

Wouldn't it be nice if I'll just right this:

```javascript
lift('grayscale2', function grayscale2(data) {
  var new_data = clone_image_data(data);

  return multiply_matrix(new_data, [ [0.21, 0.72, 0.07, 0],
                                     [0.21, 0.72, 0.07, 0],
                                     [0.21, 0.72, 0.07, 0],
                                     [0   , 0,    0,    1] ]);
});
```

Here is the first implementation of `multiply_matrix` function:

```
export function multiply_matrix(data, matrix) {
  if (matrix.length !== 4 || matrix.filter( (el) => el.length !== 4 ).length > 0)
    throw new Error("filter matrix can be only 4 by 4");

  let max = data.width * data.height;
  var tmp = [];

  for (let pos = 0; pos < max; ++pos) {
    let real_pos = pos * 4;

    for (let comp = 0; comp < 4; ++comp) {
      tmp.push(
        data.data[real_pos] * matrix[comp][0] +
        data.data[real_pos + 1] * matrix[comp][1] +
        data.data[real_pos + 2] * matrix[comp][2] +
        data.data[real_pos + 3] * matrix[comp][3]
      );
    }

    for (let comp = 0; comp < 4; ++comp) {
      data.data[real_pos + comp] = tmp[comp];
    }

    tmp.length = 0;
  }

  return data;
}
```

The main problem with it was the performance (grayscale2: around 350ms
and grayscale around 125ms on my MBP for [1024x768
image](https://github.com/can3p/img.js/blob/731b8989a49ca441508018902a7e9752123f6ad7/example/test.jpg))
and I was not really sure was it about algorythm or about js
interpreter itself.


Searching the internet revealed that matrix multiplication is not a
really
[easy](https://en.wikipedia.org/wiki/Coppersmith%E2%80%93Winograd_algorithm)
task to optimize in terms of performance and I decided to focus on js internals.

Profiler showed that Chrome was really busy with garbage collecting
and setting length of array to zero costed me around 100ms. Wow.

Ok, I've tried to assign new array at the end of each loop pass:
```javascript
var tmp = [];

for (let pos = 0; pos < max; ++pos) {
  let real_pos = pos * 4;

  for (let comp = 0; comp < 4; ++comp) {
    tmp.push(
      data.data[real_pos] * matrix[comp][0] +
      data.data[real_pos + 1] * matrix[comp][1] +
      data.data[real_pos + 2] * matrix[comp][2] +
      data.data[real_pos + 3] * matrix[comp][3]
    );
  }

  for (let comp = 0; comp < 4; ++comp) {
    data.data[real_pos + comp] = tmp[comp];
  }

  tmp = [];
}
```

The perfomance fell to miserable 550ms but very fun fact was
that profiler didn't show anything specific and garbage collection
was reducde by and order of magnitude.

Ok, let's try to go from the other side and not reset the length of
array at all. Memory is cheap.

```javascript
var tmp = [];

for (let pos = 0; pos < max; ++pos) {
  let real_pos = pos * 4;

  for (let comp = 0; comp < 4; ++comp) {
    tmp.push(
      data.data[real_pos] * matrix[comp][0] +
      data.data[real_pos + 1] * matrix[comp][1] +
      data.data[real_pos + 2] * matrix[comp][2] +
      data.data[real_pos + 3] * matrix[comp][3]
    );
  }

  for (let comp = 0; comp < 4; ++comp) {
    data.data[real_pos + comp] = tmp[real_pos + comp];
  }
}
```

And it was the first success: around 260ms! Much better but still not
as good as the original thing.

I decided not to give up and started looking for the hints about v8
interpreter internals that can give me the speedup. The
most interesting [video](https://www.youtube.com/watch?v=UJPdhx5zTaw)
I've found gave me all sorts of hints about how I can shoot myself
in a foot in terms of performance but I didn't get much after
discovering the fact the multiply_matrix function is getting
deoptimized constantly. Maybe I'll return to this later, but
one cool fact that I've discovered is that you can get the
fresh instance of the Chrome browser by just starting it
with different working directory and it will hold the version
that was with it right after install before updates happened.
Also you can pass parameters right to the v8 engine and get all
sorts of debug output.

```bash
/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --user-data-dir="/tmp/chrome_dev" --js-flags="--trace-opt --trace-deopt --trace-bailouts"
```

Here --user-data-dir option will give the fresh instance (even if there is
a running one) and --js-flags will be passed to v8.

The main point for the talk is that the more information you give to browser
about types in your code the more speedup you may theoretically gain. I remembered
that typed arrays were introduced for all sorts of low-level number crunching
and found a very good [article](https://hacks.mozilla.org/2011/12/faster-canvas-pixel-manipulation-with-typed-arrays/)
on Mozilla Hacks.

I decided to start with just Uint8ClampedArray:

```javascript
var buf = new ArrayBuffer(data.data.length);
var buf8 = new Uint8ClampedArray(buf);

for (let pos = 0; pos < max; ++pos) {
  let real_pos = pos * 4;

  for (let comp = 0; comp < 4; ++comp) {
    buf8[real_pos + comp] = (
      data.data[real_pos] * matrix[comp][0] +
      data.data[real_pos + 1] * matrix[comp][1] +
      data.data[real_pos + 2] * matrix[comp][2] +
      data.data[real_pos + 3] * matrix[comp][3]
    );
  }
}

data.data.set(buf8);
```

It gave me blazing 120ms, as fast as original, but I was curious
about using Uint32Array and decided to try it:

```javascript
var buf = new ArrayBuffer(data.data.length);
var buf8 = new Uint8ClampedArray(buf);
var buf32 = new Uint32Array(buf);

for (let pos = 0; pos < max; ++pos) {
  let real_pos = pos * 4;

  buf32[pos] = (
    ((data.data[real_pos] * matrix[3][0] + data.data[real_pos + 1] * matrix[3][1] + data.data[real_pos + 2] * matrix[3][2] + data.data[real_pos + 3] * matrix[3][3]) << 24) |
    ((data.data[real_pos] * matrix[2][0] + data.data[real_pos + 1] * matrix[2][1] + data.data[real_pos + 2] * matrix[2][2] + data.data[real_pos + 3] * matrix[2][3]) << 16) |
    ((data.data[real_pos] * matrix[1][0] + data.data[real_pos + 1] * matrix[1][1] + data.data[real_pos + 2] * matrix[1][2] + data.data[real_pos + 3] * matrix[1][3]) << 8) |
    ((data.data[real_pos] * matrix[0][0] + data.data[real_pos + 1] * matrix[0][1] + data.data[real_pos + 2] * matrix[0][2] + data.data[real_pos + 3] * matrix[0][3]))
  );
}

data.data.set(buf8);
```

I'm not detecting endiannes here just measuring speed. And measurements
gave me the incredible 20-25ms, five times faster than original function!

Probably there is a way to make even this magic number lower, but
I decided to stop there for a while.

Another speedup I got at the image rendering at the end. At first I
wanted to get a proper img tag at the end and my code looked like this:

```javascript
lift('outputTo', function outputTo(data, el) {
  var c = document.createElement('canvas');
  c.width = data.width; c.height = data.height;

  var ctx = c.getContext('2d');
  ctx.putImageData(data, 0, 0);

  var node = new Image();
  node.src = c.toDataURL("image/png");

  el.innerHTML = '';
  el.appendChild(node);
});
```

Chrome on averaged spend up to 200ms on this and at least half of this
time was spend on the toDataUrl method execution. Firefox
behaved even worse with 600ms. It was too bad and I decided
to drop image part for now and output just canvas:

```javascript
lift('outputTo', function outputTo(data, el) {
  var c = document.createElement('canvas');
  c.width = data.width; c.height = data.height;

  var ctx = c.getContext('2d');
  ctx.putImageData(data, 0, 0);

  el.innerHTML = '';
  el.appendChild(c);
});
```

And the timing went to 5ms at firefox and less the one ms in Chrome.
With all these optimisation the image processing part became barely
noticable which is very good and opens space for more advanced
filters that will run in a reasonable time now.
