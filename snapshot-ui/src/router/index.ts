// src/router/index.ts
import { createRouter, createWebHistory } from 'vue-router'
import SnapshotList from '@/views/SnapshotList.vue'  

const routes = [
  { path: '/', name: 'SnapshotList', component: SnapshotList }
]

export const router = createRouter({
  history: createWebHistory(),
  routes
})
