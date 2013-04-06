/*  -*- mode: c; tab-width: 4; -*-  */
/*  FileName    : capturer_mjpeg.c  */
/*  Author      : Wang ShuYu <andelf@gmail.com>  */
/*  Created     : Thu Apr  4 15:03:51 2013 by Wang ShuYu  */
/*  Copyright   : Feather Workshop (c) 2013  */
/*  Description : capture device, output mjpeg  */
/*  Time-stamp: <>  */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <getopt.h>             /* getopt_long() */
#include <fcntl.h>              /* low-level i/o */
#include <unistd.h>
#include <errno.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <asm/types.h>          /* for videodev2.h */
#include <linux/videodev2.h>
#include <jpeglib.h>
#include <gdfonts.h>
#include <gd.h>
#include <time.h>

#define OUTPUT_BUF_SIZE  4096
#define CLEAR(x) memset (&(x), 0, sizeof (x))
#define MAX_INPUT   16
#define MAX_NORM    16

//info needed to store one video frame in memory
struct buffer {
	void *                  start;
	size_t                  length;
};

typedef struct {
  struct jpeg_destination_mgr pub;
  JOCTET * buffer;
  unsigned char *outbuffer;
  int outbuffer_size;
  unsigned char *outbuffer_cursor;
  int *written;
} mjpg_destination_mgr;

typedef mjpg_destination_mgr *mjpg_dest_ptr;

/* output errno and exit */
static void errno_exit (const char *s)
{
	fprintf (stderr, "%s error %d, %s\n",s, errno, strerror (errno));
	exit (EXIT_FAILURE);
}


//a blocking wrapper of the ioctl function
static int xioctl (int fd, int request, void *arg)
{
	int r;

	do r = ioctl (fd, request, arg);
	while (-1 == r && EINTR == errno);

	return r;
}


static char * jpeg_add_watermark(char *buf, int size, int *outsize) {
  gdImagePtr im;
  char label[100+1];
  char *ret;
  time_t now;

  time(&now);
  strftime(label, 100, "by andelf %Y-%m-%d %H:%M:%S %Z", localtime(&now));
  im = gdImageCreateFromJpegPtr(size, buf);
  gdImageString(im, gdFontSmall,
                0, 0, label, gdImageColorAllocate(im, 255, 255, 255));
  ret = gdImageJpegPtr(im, outsize, 80);
  gdImageDestroy(im);
  return ret;
}

//read one frame from memory and throws the data to standard output
static int read_frame  (int * fd, int width, int height, int * n_buffers,
						struct buffer * buffers, int pixel_format)
{
	struct v4l2_buffer buf;//needed for memory mapping
	unsigned int i;
	unsigned int Bpf;//bytes per frame
    /* for output jpeg */
    char outbuf[800*600*3]; /* enough big output buffer */
    char *watermarked_p = NULL;
    int ret;

	CLEAR (buf);

	buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	buf.memory = V4L2_MEMORY_MMAP;

	if (-1 == xioctl (*fd, VIDIOC_DQBUF, &buf))
	{
		switch (errno)
		{
			case EAGAIN:
				return 0;

			case EIO://EIO ignored

			default:
				errno_exit ("VIDIOC_DQBUF");
		}
	}

    // fprintf(stderr, "frame bytesused: %u\n", buf.bytesused);
    /* output: 614400 */

	assert (buf.index < *n_buffers);

	switch (pixel_format)
	{
    case 0: //YUV420
        Bpf = width*height*12/8;
        fprintf(stderr, "not implemented!\n");
        break;
    case 1: //RGB565
        Bpf = width*height*2;
        fprintf(stderr, "not implemented!\n");
        break;
    case 2: //RGB32
        Bpf = width*height*4;
        fprintf(stderr, "not implemented!\n");
        break;
    case 3: //YUYV
        Bpf = width*height*2;
        if (buf.bytesused < Bpf) {
          fprintf(stderr, "partical frame, ignore\n");
        }
        else {
          ret = compress_yuyv_to_jpeg(buffers[buf.index].start, width, height, Bpf, outbuf, 80);
          watermarked_p = jpeg_add_watermark(outbuf, Bpf, &Bpf);
          ret = write(STDOUT_FILENO, watermarked_p, Bpf);
          gdFree(watermarked_p);
        }
        break;
    default:
		break;
	}

	//writing to standard output
	//ret = write(STDOUT_FILENO, buffers[buf.index].start, Bpf);

	if (-1 == xioctl (*fd, VIDIOC_QBUF, &buf))
		errno_exit ("VIDIOC_QBUF");

	return 1;
}

static void stop_capturing (int * fd)
{
	enum v4l2_buf_type type;

	type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	//this call to xioctl allows to stop the stream from the capture device
	if (-1 == xioctl (*fd, VIDIOC_STREAMOFF, &type))
		errno_exit ("VIDIOC_STREAMOFF");
}

//just the main loop of this program
static void mainloop (int * fd, int width, int height, int * n_buffers,
					struct buffer * buffers, int pixel_format)
{
	unsigned int count;
	count = 100;
	for (;;)
	{
		fd_set fds;
		struct timeval tv;
		int r;

		FD_ZERO (&fds);
		FD_SET (*fd, &fds);

		/* Select Timeout */
		tv.tv_sec = 2;
		tv.tv_usec = 0;

		//the classic select function, who allows to wait up to 2 seconds,
		//until we have captured data,
		r = select (*fd + 1, &fds, NULL, NULL, &tv);

		if (-1 == r)
		{
			if (EINTR == errno)
				continue;

			errno_exit ("select");
		}

		if (0 == r)
		{
			fprintf (stderr, "select timeout\n");
			exit (EXIT_FAILURE);
		}

		//read one frame from the device and put on the buffer
		read_frame (fd, width, height, n_buffers, buffers, pixel_format);

        int cmd = getchar();              /* wait */
        switch (cmd) {
        case 'q':
          // fprintf(stderr, "quit\n");
          // stop_capturing(fd);
          return;               /* quit func */
        default:
          break;
        }
	}
}

static void start_capturing (int * fd, int * n_buffers )
{
	unsigned int i;
	enum v4l2_buf_type type;

	for (i = 0; i < *n_buffers; ++i)
	{
		struct v4l2_buffer buf;

		CLEAR (buf);

		buf.type        = V4L2_BUF_TYPE_VIDEO_CAPTURE;
		buf.memory      = V4L2_MEMORY_MMAP;
		buf.index       = i;

		if (-1 == xioctl (*fd, VIDIOC_QBUF, &buf))
			errno_exit ("VIDIOC_QBUF");
	}

	type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	//start the capture from the device
	if (-1 == xioctl (*fd, VIDIOC_STREAMON, &type))
		errno_exit ("VIDIOC_STREAMON");
}


//free the shared memory area
static void uninit_device (int * n_buffers, struct buffer * buffers)
{
	unsigned int i;

	for (i = 0; i < *n_buffers; ++i)
		if (-1 == munmap (buffers[i].start, buffers[i].length))
			errno_exit ("munmap");
	free (buffers);
}


static void get_control(int fd) {
  struct v4l2_control ctl;      /* id(CID), value */
  ctl.id = V4L2_CID_BRIGHTNESS;
  if (-1 == ioctl(fd, VIDIOC_G_CTRL, &ctl)) {
    // errno_exit("v4l_get_picture:");
    // return ;
  }
  fprintf(stderr, "brightness: %d\n", ctl.value);
  ctl.id = V4L2_CID_CONTRAST;
  if (-1 == ioctl(fd, VIDIOC_G_CTRL, &ctl)) {
    // errno_exit("v4l_get_picture:");
    // return ;
  }
  fprintf(stderr, "contrast: %d\n", ctl.value);
  ctl.id = V4L2_CID_WHITENESS;
  if (-1 == ioctl(fd, VIDIOC_G_CTRL, &ctl)) {
    // errno_exit("v4l_get_picture:");
    // return ;
  }
  fprintf(stderr, "whiteness: %d\n", ctl.value);
}

//alloc buffers and configure the shared memory area
static struct buffer *init_mmap (int * fd, char * dev_name, int * n_buffers)
{
	struct v4l2_requestbuffers req;
	//buffers is an array of n_buffers length, and every element store a frame
	struct buffer *buffers = NULL;
	CLEAR (req);

	req.count               = 4;
	req.type                = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	req.memory              = V4L2_MEMORY_MMAP;

	if (-1 == xioctl (*fd, VIDIOC_REQBUFS, &req))
	{
		if (EINVAL == errno)
		{
			fprintf (stderr, "%s does not support "
                     "memory mapping\n", dev_name);
			exit (EXIT_FAILURE);
		} else {
			errno_exit ("VIDIOC_REQBUFS");
		}
	}

	if (req.count < 2)
	{
		fprintf (stderr, "Insufficient buffer memory on %s\n",dev_name);
		exit (EXIT_FAILURE);
	}
	buffers = calloc (req.count, sizeof (*buffers));
	if (!buffers)
	{
		fprintf (stderr, "Out of memory\n");
		exit (EXIT_FAILURE);
	}
	//map every element of the array buffers to the shared memory
	for (*n_buffers = 0; *n_buffers < req.count; ++*n_buffers)
	{
		struct v4l2_buffer buf;

		CLEAR (buf);

		buf.type        = V4L2_BUF_TYPE_VIDEO_CAPTURE;
		buf.memory      = V4L2_MEMORY_MMAP;
		buf.index       = *n_buffers;

		if (-1 == xioctl (*fd, VIDIOC_QUERYBUF, &buf))
			errno_exit ("VIDIOC_QUERYBUF");

		buffers[*n_buffers].length = buf.length;
		buffers[*n_buffers].start = mmap (NULL /* start anywhere */,
							buf.length,
							PROT_READ | PROT_WRITE /* required */,
							MAP_SHARED /* recommended */,
							*fd, buf.m.offset);

		if (MAP_FAILED == buffers[*n_buffers].start)
			errno_exit ("mmap");
	}
	return buffers;
}


static void dump_device_info (int * fd)
{
  	struct v4l2_capability cap; /* capability of webcam */
    struct v4l2_fmtdesc fmtdesc; /* format description */

    if (-1 == xioctl (*fd, VIDIOC_QUERYCAP, &cap))
	{
		if (EINVAL == errno)
		{
			fprintf (stderr, "not a V4L2 device\n");
			exit (EXIT_FAILURE);
		} else {
			errno_exit ("VIDIOC_QUERYCAP");
		}
	}

    fprintf(stderr,"\nv4l2_cap\n");
    fprintf(stderr,"\t      driver = %s\n",cap.driver);
    fprintf(stderr,"\t        card = %s\n",cap.card);
    fprintf(stderr,"\t    bus_info = %s\n",cap.bus_info);
    fprintf(stderr,"\t     version = %u.%u.%u\n",(cap.version >> 16) & 0xFF,(cap.version >> 8) & 0xFF,cap.version & 0xFF);
    fprintf(stderr,"\tcapabilities = 0x%x\n",cap.capabilities);

    if(cap.capabilities & V4L2_CAP_VIDEO_CAPTURE)  fprintf(stderr,"\t\t\t V4L2_CAP_VIDEO_CAPTURE\n");
    if(cap.capabilities & V4L2_CAP_VIDEO_OUTPUT)   fprintf(stderr,"\t\t\t V4L2_CAP_VIDEO_OUTPUT\n");
    if(cap.capabilities & V4L2_CAP_VIDEO_OVERLAY)  fprintf(stderr,"\t\t\t V4L2_CAP_VIDEO_OVERLAY\n");
    if(cap.capabilities & V4L2_CAP_VBI_CAPTURE)   fprintf(stderr,"\t\t\t V4L2_CAP_VBI_CAPTURE\n");
    if(cap.capabilities & V4L2_CAP_VBI_OUTPUT)   fprintf(stderr,"\t\t\t V4L2_CAP_VBI_OUTPUT\n");
    if(cap.capabilities & V4L2_CAP_SLICED_VBI_CAPTURE) fprintf(stderr,"\t\t\t V4L2_CAP_SLICED_VBI_CAPTURE\n");
    if(cap.capabilities & V4L2_CAP_SLICED_VBI_OUTPUT) fprintf(stderr,"\t\t\t V4L2_CAP_SLICED_VBI_OUTPUT\n");
    if(cap.capabilities & V4L2_CAP_RDS_CAPTURE)   fprintf(stderr,"\t\t\t V4L2_CAP_RDS_CAPTURE\n");
    if(cap.capabilities & V4L2_CAP_VIDEO_OUTPUT_OVERLAY) fprintf(stderr,"\t\t\t V4L2_CAP_VIDEO_OUTPUT_OVERLAY\n");
    if(cap.capabilities & V4L2_CAP_HW_FREQ_SEEK)   fprintf(stderr,"\t\t\t V4L2_CAP_HW_FREQ_SEEK\n");
    if(cap.capabilities & V4L2_CAP_RDS_OUTPUT)   fprintf(stderr,"\t\t\t V4L2_CAP_RDS_OUTPUT\n");
    if(cap.capabilities & V4L2_CAP_VIDEO_CAPTURE_MPLANE) fprintf(stderr,"\t\t\t V4L2_CAP_VIDEO_CAPTURE_MPLANE\n");
    if(cap.capabilities & V4L2_CAP_VIDEO_OUTPUT_MPLANE) fprintf(stderr,"\t\t\t V4L2_CAP_VIDEO_OUTPUT_MPLANE\n");
    if(cap.capabilities & V4L2_CAP_TUNER )    fprintf(stderr,"\t\t\t V4L2_CAP_TUNER \n");
    if(cap.capabilities & V4L2_CAP_AUDIO)    fprintf(stderr,"\t\t\t V4L2_CAP_AUDIO\n");
    if(cap.capabilities & V4L2_CAP_RADIO)    fprintf(stderr,"\t\t\t V4L2_CAP_RADIO\n");
    if(cap.capabilities & V4L2_CAP_MODULATOR)   fprintf(stderr,"\t\t\t V4L2_CAP_MODULATOR\n");
    if(cap.capabilities & V4L2_CAP_READWRITE)   fprintf(stderr,"\t\t\t V4L2_CAP_READWRITE\n");
    if(cap.capabilities & V4L2_CAP_ASYNCIO)    fprintf(stderr,"\t\t\t V4L2_CAP_ASYNCIO\n");
    if(cap.capabilities & V4L2_CAP_STREAMING)   fprintf(stderr,"\t\t\t V4L2_CAP_STREAMING\n");
    if(cap.capabilities & V4L2_CAP_DEVICE_CAPS) {
        fprintf(stderr,"\t\t\t V4L2_CAP_DEVICE_CAPS\n");
        fprintf(stderr,"\tdevice_caps = 0x%x\n", cap.device_caps);
    }

    /* capabilities dump ends */
    fmtdesc.index = 0;
    fmtdesc.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    fprintf(stderr, "supported format:\n");
    while (-1 != ioctl(*fd, VIDIOC_ENUM_FMT,&fmtdesc)) {
        // errno_exit ("VIDIOC_ENUM_FMT");
        fprintf(stderr,"\t\t      index = %x\n",fmtdesc.flags);
        fprintf(stderr,"\t\t      flags = %x\n",fmtdesc.flags);
        fprintf(stderr,"\t\tdescription = %s\n",fmtdesc.description);
        fprintf(stderr,"\t\tpixelformat = %c%c%c%c\n",
                fmtdesc.pixelformat&0xff,fmtdesc.pixelformat>>8&0xff,fmtdesc.pixelformat>>16&0xff,fmtdesc.pixelformat>>24&0xff);
        fmtdesc.index += 1;

    }
    /* format enum ends */
}

//configure and initialize the hardware device
static struct buffer *init_device (int * fd, char * dev_name, int width,
                                   int height, int * n_buffers, int pixel_format)
{
	struct v4l2_capability cap; /* capability of webcam */
	struct v4l2_cropcap cropcap;
	struct v4l2_crop crop;
	struct v4l2_format fmt;
	struct buffer * buffers = NULL;
	unsigned int min;

    if (-1 == xioctl (*fd, VIDIOC_QUERYCAP, &cap))
	{
		if (EINVAL == errno)
		{
			fprintf (stderr, "not a V4L2 device\n");
			exit (EXIT_FAILURE);
		} else {
			errno_exit ("VIDIOC_QUERYCAP");
		}
	}

    if (!(cap.capabilities & V4L2_CAP_VIDEO_CAPTURE))
    {
        fprintf (stderr, "%s is no video capture device\n",dev_name);
        exit (EXIT_FAILURE);
    }

    if (!(cap.capabilities & V4L2_CAP_STREAMING))
    {
        fprintf (stderr, "%s does not support streaming i/o\n",dev_name);
        exit (EXIT_FAILURE);
    }

    /* Select video input, video standard and tune here. */
    cropcap.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

    if (-1 == xioctl (*fd, VIDIOC_CROPCAP, &cropcap))
    {
        /* Errors ignored. */
    }

    crop.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    crop.c = cropcap.defrect; /* reset to default */

    if (-1 == xioctl (*fd, VIDIOC_S_CROP, &crop))
    {
        switch (errno) {
        case EINVAL:
            /* Cropping not supported. */
			break;
        default:
            /* Errors ignored. */
			break;
        }
    }

    CLEAR (fmt);
	//set image properties
	fmt.type                = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	fmt.fmt.pix.width       = width;
	fmt.fmt.pix.height      = height;

	switch (pixel_format)
	{
    case 0:
        fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_YUV420;
        break;
    case 1:
        fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_RGB565;
        break;
    case 2:
        fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_RGB32;
        break;
    case 3:
        fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_YUYV;
        break;
	}
	//fmt.fmt.pix.colorspace  = V4L2_COLORSPACE_SRGB;
	//fmt.fmt.pix.field       = V4L2_FIELD_INTERLACED;

	if (-1 == xioctl (*fd, VIDIOC_S_FMT, &fmt))
		errno_exit ("\nError: pixel format not supported\n");

	/* Note VIDIOC_S_FMT may change width and height. */
    fprintf(stderr, "Current data format information:\n\twidth:%d\n\theight:%d\n",
            fmt.fmt.pix.width, fmt.fmt.pix.height);

	//check the configuration data
	min = fmt.fmt.pix.width * 2;
	if (fmt.fmt.pix.bytesperline < min)
        fmt.fmt.pix.bytesperline = min;
	min = fmt.fmt.pix.bytesperline * fmt.fmt.pix.height;
	if (fmt.fmt.pix.sizeimage < min)
        fmt.fmt.pix.sizeimage = min;

	fprintf(stderr, "Video bytespreline = %d\n",fmt.fmt.pix.bytesperline);

    get_control(*fd);

	buffers = init_mmap (fd, dev_name, n_buffers);

	return buffers;
}

static void close_device (int * fd)
{
	if (-1 == close (*fd))
		errno_exit ("close");

	*fd = -1;
}

static void open_device (int * fd, char * dev_name)
{
	struct stat st;

	if (-1 == stat (dev_name, &st))
	{
		fprintf (stderr, "Cannot identify '%s': %d, %s\n",dev_name, errno, strerror (errno));
        exit (EXIT_FAILURE);
	}

	if (!S_ISCHR (st.st_mode))
	{
		fprintf (stderr, "%s is no device\n", dev_name);
        exit (EXIT_FAILURE);
	}

	*fd = open (dev_name, O_RDWR /* required */ | O_NONBLOCK, 0);

	if (-1 == *fd)
	{
		fprintf (stderr, "Cannot open '%s': %d, %s\n",dev_name, errno, strerror (errno));
		exit (EXIT_FAILURE);
	}
}


METHODDEF(void) init_destination(j_compress_ptr cinfo) {
  mjpg_dest_ptr dest = (mjpg_dest_ptr) cinfo->dest;
  dest->buffer = (JOCTET *)(*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_IMAGE, OUTPUT_BUF_SIZE * sizeof(JOCTET));
  *(dest->written) = 0;
  dest->pub.next_output_byte = dest->buffer;
  dest->pub.free_in_buffer = OUTPUT_BUF_SIZE;
}


METHODDEF(boolean) empty_output_buffer(j_compress_ptr cinfo) {
  mjpg_dest_ptr dest = (mjpg_dest_ptr) cinfo->dest;
  memcpy(dest->outbuffer_cursor, dest->buffer, OUTPUT_BUF_SIZE);
  dest->outbuffer_cursor += OUTPUT_BUF_SIZE;
  *(dest->written) += OUTPUT_BUF_SIZE;
  dest->pub.next_output_byte = dest->buffer;
  dest->pub.free_in_buffer = OUTPUT_BUF_SIZE;

  return TRUE;
}



METHODDEF(void) term_destination(j_compress_ptr cinfo) {
  mjpg_dest_ptr dest = (mjpg_dest_ptr) cinfo->dest;
  size_t datacount = OUTPUT_BUF_SIZE - dest->pub.free_in_buffer;

  /* Write any data remaining in the buffer */
  memcpy(dest->outbuffer_cursor, dest->buffer, datacount);
  dest->outbuffer_cursor += datacount;
  *(dest->written) += datacount;
}


void dest_buffer(j_compress_ptr cinfo, unsigned char *buffer, int size, int *written) {
  mjpg_dest_ptr dest;

  if (cinfo->dest == NULL) {
    cinfo->dest = (struct jpeg_destination_mgr *)(*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT, sizeof(mjpg_destination_mgr));
  }

  dest = (mjpg_dest_ptr)cinfo->dest;
  dest->pub.init_destination = init_destination;
  dest->pub.empty_output_buffer = empty_output_buffer;
  dest->pub.term_destination = term_destination;
  dest->outbuffer = buffer;
  dest->outbuffer_size = size;
  dest->outbuffer_cursor = buffer;
  dest->written = written;
}


//摄像头采集的YUYV格式转换为JPEG格式
int compress_yuyv_to_jpeg(unsigned char *inbuf, int width, int height, int size, unsigned char *outbuf, int quality) {
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;

  JSAMPROW row_pointer[1];
  unsigned char *line_buffer, *yuyv;
  int z;

  static int written;
  //int count = 0;
  /* size verify */
  if (width * height * 2 != size) {
    fprintf(stderr, "compress_yuyv_to_jpeg: width, height, size must be width * height * 2 == size !\n");
    exit(EXIT_SUCCESS);
  }

  line_buffer = calloc (width * 3, 1);

  yuyv = inbuf;//将YUYV格式的图片数据赋给YUYV指针

  cinfo.err = jpeg_std_error (&jerr);

  jpeg_create_compress (&cinfo);
  /* jpeg_stdio_dest (&cinfo, file); */
  dest_buffer(&cinfo, outbuf, size, &written);

  cinfo.image_width = width;
  cinfo.image_height = height;
  cinfo.input_components = 3;
  cinfo.in_color_space = JCS_RGB;

  jpeg_set_defaults (&cinfo);
  jpeg_set_quality (&cinfo, quality, TRUE);
  jpeg_start_compress (&cinfo, TRUE);

  z = 0;

  while (cinfo.next_scanline < height) {
    int x;
    unsigned char *ptr = line_buffer;

    for (x = 0; x < width; x++) {
      int r, g, b;
      int y, u, v;

      if (!z)
        y = yuyv[0] << 8;
      else
        y = yuyv[2] << 8;

      u = yuyv[1] - 128;
      v = yuyv[3] - 128;

      r = (y + (359 * v)) >> 8;
      g = (y - (88 * u) - (183 * v)) >> 8;
      b = (y + (454 * u)) >> 8;

      *(ptr++) = (r > 255) ? 255 : ((r < 0) ? 0 : r);
      *(ptr++) = (g > 255) ? 255 : ((g < 0) ? 0 : g);
      *(ptr++) = (b > 255) ? 255 : ((b < 0) ? 0 : b);

      if (z++) {
        z = 0;
        yuyv += 4;
      }
    }

    row_pointer[0] = line_buffer;
    jpeg_write_scanlines (&cinfo, row_pointer, 1);
  }

  jpeg_finish_compress (&cinfo);
  jpeg_destroy_compress (&cinfo);

  free (line_buffer);
  return (written);
}

/* jpeg decode ends */

//show the usage
static void usage (FILE *fp, int argc, char **argv)
{
	fprintf (fp,
				"Usage: %s [options]\n\n"
				"Options:\n"
				"-D | --device       name               Select device name [/dev/video0]\n"
				"-d | --device-info  name               Show device info\n"
				"-i | --input        number             Video input number \n"
				"-s | --standard     number             Video standard \n"
				"-w | --window-size  <640*480|          Video size\n"
				"                      320*240>\n"
				"-p | --pixel-format number             Pixel Format (0 = YUV420)\n"
				"                                                    (1 = RGB565)\n"
				"                                                    (2 = RGB32 )\n"
                "                                                    (3 = YUYV  )\n"
				"-h | --help                            Print this message\n"
				"\n",
				argv[0]);
}

//used by getopt_long to know the possible inputs
static const char short_options [] = "D:d:i:s:w:p:h";

//long version of the previous function
static const struct option
long_options [] =
{
	{ "device",      required_argument,      NULL,           'D' },
	{ "device-info", required_argument,      NULL,           'd' },
	{ "input",       required_argument,      NULL,           'i' },
	{ "standard",    required_argument,      NULL,           's' },
	{ "window-size", required_argument,      NULL,           'w' },
	{ "pixel-format",required_argument,      NULL,           'p' },
	{ "help",        no_argument,            NULL,           'h' },
	{ 0, 0, 0, 0 }
};

//show the available devices
static void enum_inputs (int * fd)
{
	int  ninputs;
	struct v4l2_input  inp[MAX_INPUT];
	printf("Available Inputs:\n");
	for (ninputs = 0; ninputs < MAX_INPUT; ninputs++)
	{
		inp[ninputs].index = ninputs;
		if (-1 == ioctl(*fd, VIDIOC_ENUMINPUT, &inp[ninputs]))
			break;
		printf("number = %d      description = %s\n",ninputs,inp[ninputs].name);
	}
}

//show the available standards(norms) for capture
static void enum_standards (int * fd )
{
	struct v4l2_standard  std[MAX_NORM];
	int  nstds;
	printf("Available Standards:\n");
	for (nstds = 0; nstds < MAX_NORM; nstds++)
	{
		std[nstds].index = nstds;
		if (-1 == ioctl(*fd, VIDIOC_ENUMSTD, &std[nstds]))
			break;
		printf("number = %d     name = %s\n",nstds,std[nstds].name);
	}
}

//configure the video input
static void set_input(int * fd, int dev_input)
{
	struct v4l2_input input;
	int index = dev_input;
	//set the input
	if (-1 == ioctl (*fd, VIDIOC_S_INPUT, &index))
	{
		perror ("VIDIOC_S_INPUT");
		exit (EXIT_FAILURE);
	}
	//check the input
	if (-1 == ioctl (*fd, VIDIOC_G_INPUT, &index))
	{
		perror ("VIDIOC_G_INPUT");
		exit (EXIT_FAILURE);
	}
	memset (&input, 0, sizeof (input));
	input.index = index;
	if (-1 == ioctl (*fd, VIDIOC_ENUMINPUT, &input))
	{
		perror ("VIDIOC_ENUMINPUT");
		exit (EXIT_FAILURE);
	}
	fprintf (stderr,"input: %s\n", input.name);
}

//configure the capture standard
static void set_standard(int * fd, int dev_standard)
{
	struct v4l2_standard standard;
	v4l2_std_id st;
	standard.index = dev_standard;;
	if (-1 == ioctl (*fd, VIDIOC_ENUMSTD, &standard))
	{
		perror ("VIDIOC_ENUMSTD");
	}
	st=standard.id;

	if (-1 == ioctl (*fd, VIDIOC_S_STD, &st))
	{
		perror ("VIDIOC_S_STD");
	}
	fprintf (stderr,"standard: %s\n", standard.name);
}







int main (int argc, char ** argv)
{
	int                 dev_standard;
	int                 dev_input;
	int                 set_inp              = 0;
	int                 set_std              = 0;
	char                *dev_name            = "/dev/video0";
	int                 fd                   = -1;
	int                 width                = 640;
	int                 height               = 480;
	int                 n_buffers;
	struct buffer       *buffers             = NULL;
	int                 index;
	int                 c;
	int                 pixel_format         = 0;

	//process all the command line arguments
	for (;;)
	{
		c = getopt_long (argc, argv,short_options, long_options,&index);

		if (-1 == c)
			break;//no more arguments (quit from for)

		switch (c)
		{
			case 0: // getopt_long() flag
				break;

			case 'D':
				dev_name = optarg;
				break;

			case 'd':
				dev_name = optarg;
				open_device (&fd,dev_name);
				printf("\n");
				printf("Device info: %s\n\n",dev_name);
				enum_inputs(&fd);
				printf("\n");
				enum_standards(&fd);
				printf("\n");
                dump_device_info(&fd);
				close_device (&fd);
				exit (EXIT_SUCCESS);
				//break;

			case 'i':
				dev_input = atoi(optarg);
				set_inp=1;
				break;

			case 's':
				dev_standard = atoi(optarg);
				set_std=1;
				break;

			case 'w':
				if (strcmp(optarg,"640*480")==0)
				{
					printf("window size 640*480\n");
					width=640;
					height=480;
				}
				if (strcmp(optarg,"320*240")==0)
				{
					printf("window size 320*240\n");
					width=320;
					height=240;
				}
				if ((strcmp(optarg,"320*240")!=0)&&(strcmp(optarg,"640*480")!=0))
				{
					printf("\nError: window size not supported\n");
					exit(EXIT_FAILURE);
				}
				break;

			case 'p':
				pixel_format=atoi(optarg);
				break;

			case 'h':
				usage (stdout, argc, argv);
				exit (EXIT_SUCCESS);

			default:
				usage (stderr, argc, argv);
				exit (EXIT_FAILURE);
		}
	}

	open_device (&fd, dev_name);

	//set the input if needed
	if (set_inp==1)
		set_input(&fd, dev_input);

	//set the standard if needed
	if (set_std==1)
		set_standard(&fd, dev_standard);

	buffers = init_device (&fd, dev_name, width, height, &n_buffers, pixel_format);

	start_capturing (&fd, &n_buffers);

	mainloop (&fd, width, height, &n_buffers, buffers, pixel_format);

	//TODO: main loop never exits, a break method must be implemented to execute
	//the following code

	stop_capturing (&fd);

	uninit_device (&n_buffers, buffers);

	close_device (&fd);

	exit (EXIT_SUCCESS);

}
