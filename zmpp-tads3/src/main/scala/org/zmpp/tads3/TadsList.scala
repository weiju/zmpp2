/*
 * Created on 2010/10/22
 * Copyright (c) 2010, Wei-ju Wu.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * Neither the name of Wei-ju Wu nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package org.zmpp.tads3

import org.zmpp.base._

// Function table of the reference list implementation
// int (*CVmObjList::func_table_[])(VMG_ vm_val_t *retval,
//                                  const vm_val_t *self_val,
//                                  const char *lst, uint *argc) =
// {
//     &CVmObjList::getp_undef,
//     &CVmObjList::getp_subset,
//     &CVmObjList::getp_map,
//     &CVmObjList::getp_len,
//     &CVmObjList::getp_sublist,
//     &CVmObjList::getp_intersect,
//     &CVmObjList::getp_index_of,
//     &CVmObjList::getp_car,
//     &CVmObjList::getp_cdr,
//     &CVmObjList::getp_index_which,
//     &CVmObjList::getp_for_each,
//     &CVmObjList::getp_val_which,
//     &CVmObjList::getp_last_index_of,
//     &CVmObjList::getp_last_index_which,
//     &CVmObjList::getp_last_val_which,
//     &CVmObjList::getp_count_of,
//     &CVmObjList::getp_count_which,
//     &CVmObjList::getp_get_unique,
//     &CVmObjList::getp_append_unique,
//     &CVmObjList::getp_append,
//     &CVmObjList::getp_sort,
//     &CVmObjList::getp_prepend,
//     &CVmObjList::getp_insert_at,
//     &CVmObjList::getp_remove_element_at,
//     &CVmObjList::getp_remove_range,
//     &CVmObjList::getp_for_each_assoc
// };

// int CVmObjList::const_get_prop(VMG_ vm_val_t *retval,
//                                const vm_val_t *self_val, const char *lst,
//                                vm_prop_id_t prop, vm_obj_id_t *src_obj,
//                                uint *argc)
// {
//     uint func_idx;

//     // presume no source object
//     *src_obj = VM_INVALID_OBJ;

//     // translate the property index to an index into our function table
//     func_idx = G_meta_table
//                ->prop_to_vector_idx(metaclass_reg_->get_reg_idx(), prop);
    
//     // call the appropriate function
//     if ((*func_table_[func_idx])(vmg_ retval, self_val, lst, argc))
//         return TRUE;

//      // If this is a constant list (which is indicated by a non-object type
//      //  'self'), try inheriting the default object interpretation, passing
//      //  the constant list placeholder object for its type information.  
//     if (self_val->typ != VM_OBJ)
//     {
//         // try going to our base class, CVmCollection
//         if (((CVmObjCollection *)vm_objp(vmg_ G_predef->const_lst_obj))
//             ->const_get_coll_prop(vmg_ prop, retval, self_val, src_obj, argc))
//             return TRUE;

//         // try going to our next base class, CVmObject
//         if (vm_objp(vmg_ G_predef->const_lst_obj)
//             ->CVmObject::get_prop(vmg_ prop, retval, G_predef->const_lst_obj,
//                                   src_obj, argc))
//             return TRUE;
//     }
//     // not handled
//     return FALSE;
// }

class TadsList(id: TadsObjectId, metaClass: MetaClass)
extends TadsObject(id, metaClass) {

  override def toString = {
    "List object"
  }
}

class ListMetaClass   extends MetaClass {
  def name = "list"
}
