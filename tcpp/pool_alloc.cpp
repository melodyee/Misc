#include "pool_alloc.hpp"
#include <stdio.h>


#define ADDR int
#define SEGMENT_SIZE  (16384-20)  //16k
#define SEGMENT_ALIGNMENT 16384   //16k    
#define PID_MAX    6 

pool_type pools[PID_MAX];     //init???
char  addr2pid[256*1024];  //   4G/16k; one element for one page

#define FmtAssert(Cond, Str) \
    ( Cond ? (void) 1 \
           : ( printf(Str), \
               exit(0) ) )

#ifdef LRQ_DEBUG
#define Is_True FmtAssert
#else
#define Is_True(a, b) ((void) 1)
#endif


XERCES_CPP_NAMESPACE_BEGIN


// assign a new pool
static void segment_new(int pid, int size) {

#if defined(LRQ_DEBUG)
    printf("LRQ:New a segment for POOL %d: .\n", pid);
#endif

    Is_True(pid>0 && pid<=PID_MAX, ("Bad pid in segment new.\n"));

    segment *fp = (segment*) memalign(SEGMENT_ALIGNMENT, SEGMENT_SIZE);
    Is_True(fp!=NULL, ("Fail memalign in segment new."));

    fp->free_list_head = 0;
    fp->object_start = (ADDR)(fp + sizeof(segment));
    fp->object_size = size;
    fp->object_num = 0;
    fp->pid = pid;

    Is_True(addr2pid[(ADDR)fp/SEGMENT_ALIGNMENT]==0,("Bad addr2pid in segment new.\n"));
    addr2pid[(ADDR)fp/SEGMENT_ALIGNMENT] = pid;
    pools[pid-1].cur_seg = fp;
}


/*!
\brief delete a block of pool pid. if it's the newest page of pid, skip
*/
static void segment_delete(segment *seg) {

    int pid = addr2pid[(ADDR)seg/SEGMENT_ALIGNMENT];
    Is_True(pid==seg->pid, ("Seg with bad pid in segment delete.\n"));
	
#if defined(LRQ_DEBUG)
    printf("LRQ:Segment free for POOL %d: .\n", pid);
#endif

    if (pools[pid-1].cur_seg==seg)
        return;
    else {
        addr2pid[(ADDR)seg/SEGMENT_ALIGNMENT] = 0;
	 free(seg); 
    }
}


/*!
\brief malloc a memory object from the segment
*/
static void* segment_malloc(segment *fp, int size) {
     Is_True(fp!=NULL,("Bad fp in segment malloc.\n"));

    // from free list
    ADDR ret = fp->free_list_head;
    if (ret!=0) {
        Is_True( ret > (ADDR)fp, ("Bad free list in segment malloc.\n"));
        Is_True( ret + fp->object_size <= (ADDR)fp + SEGMENT_SIZE,
			("Bad free list in segment malloc.\n"));
        fp->free_list_head = *(ADDR*)ret;
        fp->object_num ++;
        return (void*)ret;
    }

    // from tail
    ret = fp->object_start;
    Is_True( ret > (ADDR)fp,  ("Bad free list in segment malloc.\n"));
    if ( ret +  fp->object_size <=(ADDR) fp + SEGMENT_SIZE ) {
        fp->object_start += fp->object_size;
        fp->object_num ++;
        return (void*)ret;
    }

// full, fail
return NULL;
}


/*!
\brief free a memory object from the segment. return true if the block
is empty
*/
static bool segment_free(segment *fp, void *ptr) 
{
    Is_True( (ADDR)ptr >= (ADDR)fp + sizeof(segment), ("Bad ptr in segment free.\n"));
    Is_True( (ADDR)ptr + fp->object_size <= (ADDR)fp + SEGMENT_SIZE,
		("Bad ptr in segment free.\n"));
    Is_True( fp->object_num>0, ("Bad segment in segment free.\n"));
    //Is_True( ((ADDR)ptr - (ADDR)fp - sizeof(segment))%fp->object_size==0,
   //	("Bad segment in segment free.\n"));
	
    // the last allocated object, move backward the object_start
    if ( (ADDR)ptr == fp->object_start - fp->object_size ) { 
        fp->object_start -= fp->object_size;
        fp->object_num --;
    }
    else { // into the free list
        *(ADDR*)ptr = fp->free_list_head;
        fp->free_list_head = (ADDR)ptr;
        fp->object_num --;
    }

    // return true if the pool is empty
    if (fp->object_num == 0) {
        fp->free_list_head = 0;
        fp->object_start = (ADDR)fp + sizeof(segment);
        return true;
    }
    else
        return false;
}

void* 
my_malloc(int pid, int size)
{
    if (pools[pid-1].size == 0) {
        Is_True(pools[pid-1].cur_seg == NULL, 
	     ("Bad pool in my_malloc.\n"));
	pools[pid-1].size = size;
	segment_new(pid, size);
    } else {
        Is_True(pools[pid-1].size == size, ("Bad pool size in my_malloc.\n"));
    }

    Is_True(pools[pid-1].cur_seg != NULL, ("Bad pool seg in my_malloc.\n"));

#if defined(LRQ_DEBUG)
    printf("LRQ: allocate for POOL %d: SIZE: %d", pid, size);
#endif

    if ((pools[pid-1].cur_seg->object_start+  size) >= ((ADDR) pools[pid-1].cur_seg + SEGMENT_SIZE))
    {
        segment_new(pid, size);
    }
    Is_True(pools[pid-1].cur_seg != NULL, ("Bad pool seg in my malloc.\n"));
    // don't use the free_list of the other segments in the pool
    void *ptr = segment_malloc(pools[pid-1].cur_seg, size);
    Is_True(ptr!=NULL, ("Bad malloc in my_malloc.\n"));
	
#if defined(LRQ_DEBUG)
    printf(" PTR:0x%x.\n", ptr);
#endif	
    return ptr;
	
}

bool
my_free(void* p)
{
    int pid = addr2pid[(int)p/SEGMENT_ALIGNMENT];
    if (pid == 0) return false;

#if defined(LRQ_DEBUG)
    printf("LRQ:Object free for POOL %d: P: 0x%x.\n", pid, p);
#endif
    segment *seg = (segment *)(((ADDR)p)-((ADDR)p & (SEGMENT_ALIGNMENT -1)));
    if (segment_free(seg, p)) {
		
#if defined(LRQ_DEBUG)
    printf("LRQ:Segment free for POOL %d: .\n", pid);
#endif
        segment_delete(seg);
    }
    return true;
    
}



class MemoryManagerImpl1 lrqmemorymanager1;
class MemoryManagerImpl2 lrqmemorymanager2;
class MemoryManagerImpl3 lrqmemorymanager3;
class MemoryManagerImpl4 lrqmemorymanager4;

void* MemoryManagerImpl1::allocate(size_t size)
{
    return my_malloc(1, size);
}

void MemoryManagerImpl1::deallocate(void* p)
{
    my_free(p);
}

void* MemoryManagerImpl2::allocate(size_t size)
{
    return my_malloc(2, size);
}

void MemoryManagerImpl2::deallocate(void* p)
{
    my_free(p);
}

void* MemoryManagerImpl3::allocate(size_t size)
{
    return my_malloc(3, size);
}

void MemoryManagerImpl3::deallocate(void* p)
{
    my_free(p);
}

void* MemoryManagerImpl4::allocate(size_t size)
{
    return my_malloc(6, size);
}

void MemoryManagerImpl4::deallocate(void* p)
{
    my_free(p);
}

XERCES_CPP_NAMESPACE_END

void
my_mem_init(void)
{
#if defined(LRQ_DEBUG)
    printf("LRQ:my_mem_ini BEGIN.\n");
#endif

    for(int i = 0; i< PID_MAX; i++) {
        pools[i].size = 0;
	 pools[i].cur_seg = NULL;
    }
    bzero(addr2pid, 256*1024*sizeof(char));

#if defined(LRQ_DEBUG)
    printf("LRQ:my_mem_ini END.\n");
#endif
}

